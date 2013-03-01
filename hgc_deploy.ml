(*
Program to control the deployment of a Mercury container onto a machine.
*)
open Core.Std;;
open Sys;;
open Unix;;

open Hgc_util;;

(* This stuff cannot be changed. *)
let realuid = getuid ()
let euid = geteuid ()

module ContainerConfig : Hgc_container.Config = struct

  let aufs_union_loc = (getenv_exn "HOME")^"/cvmfs"
  let aufs_rw_loc = "/tmp/hgc/overlay"
  let container_mount_loc = "/mnt"

end

module Container = Hgc_container.Make(ContainerConfig)

module InstanceConfig = struct

  (* Place holder for root directory - will be replaced in LXC config.*)
  let container_root_directory = "/var/lib/lxc/archibald"
  (* Place holder for user name - will be replaced in container init. *)
  let container_user_name = "thetis"

  let console_login_file = 
    ContainerConfig.aufs_union_loc ^ "/rootfs/etc/systemd/system/console-getty.service"
  ;;

  let resources = ref []

  let addResource path = resources := Container.(
  {
    device = path;
    directory = ContainerConfig.(
      aufs_union_loc^container_mount_loc^"/"^(Filename.basename path));
    fstype = "none";
    opts = ["bind"];
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

  open ContainerConfig
  open InstanceConfig

  (*  Configure the container in various ways: *)
  (* 1. Overlay the AUFS image. *)
  (* 1.5. Modify the config to point to the overlaid rootfs. *)
  (* 2. Add the user into /etc/passwd. *)
  (* 3. Set up auto-boot into a console for the user. *)
  (* 4. Scramble the root password? *)
  let configure_container () =
    let open Result in
    let open Result.Monad_infix in
    let open Pipe_infix in
    let repoint_config () =
      let config_file = aufs_union_loc^"/config" in
      let esc = unstage (String.Escaping.escape ['/'] '\\') in
      Shell.exec_wait "sed" [
        "-i";
        "s/"^(esc container_root_directory)^"/"^(esc aufs_union_loc)^"/"; 
        config_file
      ] |>
      map_error ~f:(fun _ -> "Unable to repoint config file.") 
    in
    let add_user () = 
      let login = (Passwd.getbyuid_exn realuid) in
      let gen_passwd pwd_t = Passwd.(Printf.sprintf 
        "%s:%s:%d:%d:%s:%s:%s\n" pwd_t.name pwd_t.passwd 
        pwd_t.uid pwd_t.gid pwd_t.gecos pwd_t.dir pwd_t.shell) in
      let gen_shadow pwd_t = Passwd.(Printf.sprintf 
        "%s:%s:%d:%d:%d:%d:::\n" pwd_t.name "*" 
        ((Float.to_int (time ())) / 86400) 0 99999 7) in
      (try_with 
        (fun _ -> Out_channel.with_file ~append:true (aufs_union_loc^"/rootfs/etc/passwd") 
          ~f:(fun t -> Out_channel.output_string t (gen_passwd login))) |>
        map_error ~f:Exn.to_string) >>= fun _ -> 
      (try_with 
        (fun _ -> Out_channel.with_file ~append:true (aufs_union_loc^"/rootfs/etc/shadow") 
          ~f:(fun t -> Out_channel.output_string t (gen_shadow login))) |>
        map_error ~f:Exn.to_string) >>= fun _ ->
      (try_with 
        (fun _ -> mkdir_p (aufs_union_loc^"/rootfs/home/"^login.Passwd.name)) |>
        map_error ~f:Exn.to_string) |>
      map ~f:(fun _ -> login.Passwd.name) |> 
      map_error ~f:(fun _ -> "Unable to add user.") 
    in
    let add_auto_boot username = 
      let status = Shell.exec_wait "sed" [
        "-i";
        "s/"^container_user_name^"/"^username^"/";
        console_login_file
      ] in
      map_error status ~f:(fun _ -> "Unable to set up auto-boot.") 
    in
    repoint_config () >>= fun _ ->
    add_user () >>= fun name ->
    add_auto_boot name
  ;;
end

let deploy template_loc = 
  if euid = 0 then begin
    print_string ("UID "^(Int.to_string realuid)^"\n");
    print_string ("EUID "^(Int.to_string euid)^"\n");
    print_string ("Login: "^getlogin ()^"\n");
    print_string ("Resources: "^(
      List.fold ~init:"" !InstanceConfig.resources 
      ~f:(fun x a -> x^"\n\t"^a.Container.directory))^"\n"
    );
    Random.init (Float.to_int (time ()));
    setuid 0;
    let open Result.Monad_infix in
    let container_desc = Container.({
      container_name = "archibald";
      template_location = template_loc;
      mountpoints = !InstanceConfig.resources; 
    }) in
    let status = 
      Verify.check_template template_loc >>= fun _ ->
      Container.in_container container_desc (fun () ->
        begin
          Configure.configure_container ();
 (*          Shell.fork_exec "lxc-start" [
            "-n";container_desc.Container.container_name;
            "-f";ContainerConfig.aufs_union_loc^"/config";
          ] *)
        end
      )
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
  ("-r", Arg.String (fun r -> InstanceConfig.addResource r), ": specify resource to stage in.")
]

let () =
Arg.parse speclist (fun x -> deploy x) usage
;;