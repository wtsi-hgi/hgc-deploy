(*
Program to control the deployment of a Mercury container onto a machine.
*)

open Core.Std;;
open Sys;;
open Unix;;

open Hgc_util;;

module Config = struct

  (* This stuff cannot be changed. *)
  let realuid = getuid ()
  let euid = geteuid ()

  let container_mount_location = "/mnt"

  (* Place holder for root directory - will be replaced in LXC config.*)
  let container_root_directory = "/var/lib/lxc/archibald"
  (* Place holder for user name - will be replaced in container init. *)
  let container_user_name = "thetis"

  type mountpoint = {
    device : string;
    directory : string;
    fstype : string;
    opts : string list;
  }

  let resources = ref []

  let addResource path = resources := (
    {
      device = path;
      directory = container_mount_location^"/"^(Filename.basename path);
      fstype = "none";
      opts = ["-o bind"];
    }
  ) :: !resources


end


(*
Tools to verify that a container is:
- Sane
- Safe
- Comes from an appropriate source
*)
module Verify : sig

(*
Need to verify various things about a template.
1. Needs to be in a list of approved templates - i.e. people can't just get the system
to boot a random one of their own. Either we accomplish this through signatures or
through maintaining a specific list somewhere.
2. Verify that it has a config and fstab.
3. Maybe check that the config file points to an appropriate place? 
*)
val check_template : string -> (unit, string) Result.t

end = struct

  (* Allowed locations for CVMFS images *)
  let cvmfs_location = List.map ~f:Filename.parts [
    "/cvmfs/ripley.repo";
    "/cvmfs/hgi.repo";
    "/var/lib/lxc"
  ];;

  let file_test test file msg = match (test file) with
  | `Yes -> Ok ()
  | `Unknown -> Error ("Cannot determine status of required file: "^file^"\n")
  | `No -> Error msg

  let check_location to_check against_list =
    let chk_parts = Filename.parts to_check in
    let chk_single_loc against =
      List.for_all (List.partial_zip chk_parts against) (fun (x,y) -> x = y) 
    in
    List.exists against_list chk_single_loc 
  ;;

  let check_template template =
    let open Result.Monad_infix in
    let fs_status =
      file_test file_exists template "Template directory not found." >>= fun _ ->
      file_test is_directory template "Template directory not a directory." >>= fun _ ->
      file_test file_exists (template ^ "/fstab") "fstab not found" >>= fun _ ->
      file_test file_exists (template ^ "/rootfs") "fstab not found" >>= fun _ ->
      file_test file_exists (template ^ "/config") "config not found"
    in
    let location_okay =
      if check_location template cvmfs_location then Ok () else
      Error "Template is stored in disallowed location." 
    in
    fs_status >>= fun _ ->
    location_okay
  ;;

end

(*
Tools for configuring a module.
*)
module Configure = struct

  include Config

  let mount_loc = (getenv_exn "HOME")^"/cvmfs"

  let console_login_file = 
    mount_loc ^ "/rootfs/etc/systemd/system/console-autologin.service"
  ;;

  let overlay_location template tmp_loc= {
    device = "none";
    directory = mount_loc;
    fstype = "aufs";
    opts = [Printf.sprintf "dirs=%s:%s" tmp_loc template]
  }

  (*  Configure the container in various ways: *)
  (* 1. Overlay the AUFS image. *)
  (* 1.5. Modify the config to point to the overlaid rootfs. *)
  (* 2. Add the user into /etc/passwd. *)
  (* 3. Set up auto-boot into a console for the user. *)
  (* 4. Scramble the root password? *)
  let configure_container template =
    let open Result in
    let open Result.Monad_infix in
    let open Pipe_infix in
    let tmp_loc = "/tmp/hgc/overlay_"^(string_of_int (Random.bits ())) in
    let create_locations () = 
      map_error (try_with (fun _ -> mkdir_p tmp_loc)) Exn.to_string >>= fun _ ->
      map_error (try_with (fun _ -> mkdir_p mount_loc)) Exn.to_string
    in
    let mount_succ () =
      let ol = overlay_location template tmp_loc in
      let mount_opts = (List.fold ol.opts ~init:"" ~f:(fun x y -> x ^ y)) in
      let res = Shell.run "mount" ["-t"^ol.fstype;
      ol.device;
      ol.directory;
      "-o"^mount_opts] in
      map_error res.Shell.Result.status (fun _ -> "Unable to mount device :\n"^
        ol.device^"\n"^
        ol.directory^"\n"^
        ol.fstype^"\n"^
        mount_opts^"\n"^
        res.Shell.Result.stderr
        ) 
    in
    let repoint_config () =
      let config_file = mount_loc^"/config" in
      let esc = unstage (String.Escaping.escape ['/'] '\\') in
      Shell.exec_wait "sed" [
        "-i";
        "s/"^(esc Config.container_root_directory)^"/"^(esc mount_loc)^"/"; 
        config_file
      ] |>
      map_error ~f:(fun _ -> "Unable to repoint config file.") 
    in
    let add_user () = 
      let login = (Passwd.getbyuid_exn Config.realuid) in
      let gen_passwd pwd_t = Passwd.(Printf.sprintf 
        "%s:%s:%d:%d:%s:%s:%s\n" pwd_t.name pwd_t.passwd 
        pwd_t.uid pwd_t.gid pwd_t.gecos pwd_t.dir pwd_t.shell) in
      let gen_shadow pwd_t = Passwd.(Printf.sprintf 
        "%s:%s:%d:%d:%d:%d:::\n" pwd_t.name "*" 
        ((Float.to_int (time ())) / 86400) 0 99999 7) in
      (try_with 
        (fun _ -> Out_channel.with_file ~append:true (mount_loc^"/rootfs/etc/passwd") 
          ~f:(fun t -> Out_channel.output_string t (gen_passwd login))) |>
        map_error ~f:Exn.to_string) >>= fun _ -> 
      (try_with 
        (fun _ -> Out_channel.with_file ~append:true (mount_loc^"/rootfs/etc/shadow") 
          ~f:(fun t -> Out_channel.output_string t (gen_shadow login))) |>
        map_error ~f:Exn.to_string) >>= fun _ ->
      (try_with 
        (fun _ -> mkdir_p (mount_loc^"/rootfs/home/"^login.Passwd.name)) |>
        map_error ~f:Exn.to_string) |>
      map ~f:(fun _ -> login.Passwd.name) |> 
      map_error ~f:(fun _ -> "Unable to add user.") 
    in
    let add_auto_boot username = 
      let status = Shell.exec_wait "sed" [
        "-i";
        "s/"^Config.container_user_name^"/"^username^"/";
        console_login_file
      ] in
      map_error status ~f:(fun _ -> "Unable to set up auto-boot.") 
    in
    create_locations () >>= fun _ ->
    mount_succ () >>= fun _ ->
    repoint_config () >>= fun _ ->
    add_user () >>= fun name ->
    add_auto_boot name
  ;;
end

let deploy template_loc = 
  if Config.euid = 0 then begin
    print_string ("UID "^(Int.to_string Config.realuid)^"\n");
    print_string ("EUID "^(Int.to_string Config.euid)^"\n");
    print_string ("Login: "^getlogin ()^"\n");
    Random.init (Float.to_int (time ()));
    setuid 0;
    let open Result.Monad_infix in
    let status = 
      Verify.check_template template_loc >>= fun _ ->
      Configure.configure_container template_loc
    in
    match status with
    | Ok _ -> exit 0
    | Error err -> begin
      output_string (out_channel_of_descr stderr) (err^"\n");
      exit 1
    end
  end 
else begin
  output_string (out_channel_of_descr stderr) "Must be run as suid root.\n";
  exit 1
end
;;

let usage = "usage: hg-deploy template [-r resource]*"

(* Currently there are no options really *)
let speclist = [
  ("-r", Arg.String (fun r -> Config.addResource r), ": specify resource to stage in.")
]

let () =
Arg.parse speclist (fun x -> deploy x) usage
;;