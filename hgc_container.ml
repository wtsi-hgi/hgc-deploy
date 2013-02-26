module type Config = sig

  (* Where to use for the union filesystem. *)
  val aufs_union_loc : string;
  (* Where to stash changes to the union fs. *)
  val aufs_rw_loc : string; 

end

module type S = sig

  val aufs_union_loc : string;
  val aufs_rw_loc : string; 

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

  (* Create the specified container and execute a given function in its context, ensuring that it's
    closed properly afterwards. *)
val inContainer : config -> (t -> 'a) -> 'a 

end

module Make(C : Config) : S = struct

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
    map_error ~f:err_msg |>
    map ~f:(fun _ -> mountpoint.directory)
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



      (* Public methods *)
      let inContainer conf f = 
        f (clone conf)

      end