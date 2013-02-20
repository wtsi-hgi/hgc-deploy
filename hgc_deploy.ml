(*
Program to control the deployment of a Mercury container onto a machine.
*)

open Core.Std;;
open Sys;;
open Unix;;

open Hgc_util;;


module Common = struct 

  type mountpoint = {
    device : string;
    directory : string;
    fstype : string;
    opts : string list;
  }

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

  include Common

  (* Allowed locations for CVMFS images *)
  let cvmfs_location = List.map ~f:Filename.parts ["/cvmfs/hgi.repo";"/var/lib/lxc"];;

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
      file_test file_exists (template ^ "/config") "config not found" >>| fun _ ->
      ()
    in
    let location_okay =
      if check_location template cvmfs_location then Ok () else
      Error "Template is stored in disallowed location." 
    in
    fs_status >>= fun _ ->
    location_okay >>| fun _ ->
    ()
  ;;

end

(*
Tools for configuring a module.
*)
module Configure = struct

  include Common

  let console_login_file template = 
    template ^ "/rootfs/etc/systemd/system/console-autologin.service"
  ;;

  let overlay_location template mount_loc tmp_loc= {
    device = "none";
    directory = mount_loc;
    fstype = "aufs";
    opts = [Printf.sprintf "dirs=%s:%s/rootfs/" tmp_loc template]
  }

  (*  Configure the container in various ways: *)
  (* 1. Overlay the AUFS image.*)
  (* 2. Add the user into /etc/passwd.*)
  (* 3. Set up auto-boot into a console for the user. *)
  (* 4. Scramble the root password? *)

  let configure_container template mountpoints =
    let open Result in
    let open Result.Monad_infix in
    let tmp_loc = "/tmp/hgc/overlay_"^(string_of_int (Random.bits ())) in
    let mount_loc = (getenv_exn "HOME")^"/cvmfs" in
    let create_locations () = 
      map_error (try_with (fun _ -> mkdir_p tmp_loc)) Exn.to_string >>= fun _ ->
      map_error (try_with (fun _ -> mkdir_p mount_loc)) Exn.to_string
    in
    let mount_succ () =
      let ol = overlay_location template mount_loc tmp_loc in
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
    let add_user () = 
      let open Pipe_infix in
      let login_name = getlogin () in
      let realuid = Int.to_string (getuid ()) in 
      let realgid = Int.to_string (getgid ()) in
      let status = Shell.exec_wait "adduser" ["-m"; "-u "^realuid; "-g "^realgid; login_name] in
      map status ~f:(fun _ -> login_name) >| 
      map_error ~f:(fun _ -> "Unable to add user.") 
    in
    let add_auto_boot username = 
      let cf = console_login_file template in
      let status = Shell.exec_wait "sed" ["-i";"s/thetis/"^username;cf] in
      map_error status ~f:(fun _ -> "Unable to set up auto-boot.") 
    in
    create_locations () >>= fun _ ->
    mount_succ () >>= fun _ ->
    add_user () >>= fun name ->
    add_auto_boot name >>| fun _ ->
    () 
  ;;

end


(* Some temporary variables which will eventually be passed in in some way. *)
let template_loc = "/var/lib/lxc/archibald";;
let mountpoints = Common.([
  { 
    device = "/tmp/foo";
    directory = "/tmp/foo";
    fstype = "none";
    opts = ["bind"];
  }
])


(*
Program proper.
*)
let usage () =
  print_string "Usage: hg-deploy foo bar baz"
;;

let () =
let realuid = getuid () in
let euid = geteuid () in
if euid = 0 then begin
  print_string ("UID "^(Int.to_string realuid)^"\n");
  print_string ("EUID "^(Int.to_string euid)^"\n");
  print_string ("Login: "^getlogin ()^"\n");
  setuid 0;
  let open Result.Monad_infix in
  let status = 
    Verify.check_template template_loc >>= fun _ ->
    Configure.configure_container template_loc mountpoints >>| fun _ ->
    () 
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