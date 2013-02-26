open Core.Std
open Hgc_util
open Sys
open Unix

module type Config = sig

  (* Where to use for the union filesystem. *)
  val aufs_union_loc : string
  (* Where to stash changes to the union fs. *)
  val aufs_rw_loc : string
  (* Path inside the container to bind mount resources. *)
  val container_mount_loc : string

end

module type S = sig

  (*  Mountpoint type.  *)
  type mountpoint

  (*    Configuration for a container.    *)
  type config

  (* Configured container type *)
  type t

  (* Create the specified container and execute a given function in its context, ensuring that it's
    closed properly afterwards. *)
val inContainer : config -> f:(t -> 'a) -> ('a, string) Result.t

end

module Make(C : Config) : S = struct

  (*  Mountpoint type.  *)
  type mountpoint = {
    device : string;
    directory : string;
    fstype : string;
    opts : string list;
  }

  (*    Configuration for a container.    *)
  type config = {
    container_name : string;
    template_location : string;
    mountpoints : mountpoint list;
  }

  type t = {
    aufs_union_loc : string;
    config_path : string;
    fstab_path : string;
    rootfs_path : string;
    resources : string list;
  }

  (* Private methods *)
  (* Build the overlay mount *)
  let overlay_location template tmp_loc= {
    device = "none";
    directory = C.aufs_union_loc;
    fstype = "aufs";
    opts = [Printf.sprintf "dirs=%s:%s" tmp_loc template]
  }

  (* Mount the specified point - returns a Result containing the directory name. *)
  let mount mountpoint =
    let open Result in
    let open Hgc_util.Pipe_infix in
    let opts = (List.fold mountpoint.opts ~init:"" ~f:(fun x y -> x ^ y)) in
    let err_msg result = "Unable to mount device :\n"^
    mountpoint.device^"\n"^
    mountpoint.directory^"\n"^
    mountpoint.fstype^"\n"^
    opts^"\n"^
    result.Shell.Result.stderr in
    let res = Shell.run "mount" [
      "-t"^mountpoint.fstype;
      mountpoint.device;
      mountpoint.directory;
      "-o"^opts
    ] in
    res.Shell.Result.status |> 
    map ~f:(fun _ -> mountpoint.directory) |>
    map_error ~f:(fun _ -> err_msg res)
  ;;

  (* Create a clone of the specified container. *)
  let clone conf = 
    let open Result in
    let open Result.Monad_infix in
    let open Hgc_util.Pipe_infix in
    let tmp_loc = C.aufs_rw_loc^(string_of_int (Random.bits ())) in
    let create_locations () = 
      map_error (try_with (fun _ -> mkdir_p tmp_loc)) Exn.to_string >>= fun _ ->
      map_error (try_with (fun _ -> mkdir_p C.aufs_union_loc)) Exn.to_string
    in
    let mount_overlay () =
      let ol = overlay_location conf.template_location tmp_loc in
      mount ol
    in
    let mount_resources () =
      conf.mountpoints |>
      List.map ~f:(fun x -> mount x) |>
      List.fold ~init:(return []) ~f:(fun acc x ->  
        acc >>= fun acc' -> 
        x >>= fun x' -> 
        return (x' :: acc')
        )
    in
    create_locations () >>= fun _ ->
    mount_overlay () >>= fun dir ->
    mount_resources () >>| fun dirs ->
    {
      aufs_union_loc = C.aufs_union_loc;
      config_path = C.aufs_union_loc^"/config";
      fstab_path = C.aufs_union_loc^"/fstab";
      rootfs_path = C.aufs_union_loc^"/rootfs";
      resources = (dir :: dirs);
    }
  ;;

  (* Public methods *)
  let inContainer conf ~f = 
    let container = clone conf in
    let result = Result.map container ~f in
    begin
      result  
    end
  ;;
  
end