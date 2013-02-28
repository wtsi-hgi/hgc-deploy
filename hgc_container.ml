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

  (* Create the specified container and execute a given function in its context, 
    ensuring that it's closed properly afterwards. *)
val inContainer : config -> f:(unit -> 'a) -> ('a, string) Result.t

end

module Make(C : Config) : S = struct

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

  module Mount : sig

   val with_mount : mountpoint -> (unit -> 'a) -> ('a, string) Result.t
   val with_mounts : mountpoint list -> (unit -> 'a) -> ('a, string) Result.t
 end = struct

  (* Hide this function *)
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

  let unmount dir =
    Shell.run "umount" [dir]
  ;;

  let with_mount mountpoint f = match mount mountpoint with
  | Ok dir -> let result = f () in
  begin
    unmount dir;
    Ok result
  end
  | Error a -> Error a

  let with_mounts mountpoints f =
    let open Result in
    let f' = List.fold mountpoints ~init:f ~f:(fun acc x -> match with_mount x acc with
      | Ok x' -> (fun () -> x')
      | _ -> acc) in
    return (f' ())
  ;;

end

(* Private methods *)
(* Build the overlay mount *)
let overlay_location template tmp_loc= Mount.({
  device = "none";
  directory = C.aufs_union_loc;
  fstype = "aufs";
  opts = [Printf.sprintf "dirs=%s:%s" tmp_loc template]
})

(* Public methods *)
let inContainer conf ~f = 
  let open Result in
  let open Result.Monad_infix in
  let open Hgc_util.Pipe_infix in
  let tmp_loc = C.aufs_rw_loc^(string_of_int (Random.bits ())) in
  let create_locations () = 
    map_error (try_with (fun _ -> mkdir_p tmp_loc)) Exn.to_string >>= fun _ ->
    map_error (try_with (fun _ -> mkdir_p C.aufs_union_loc)) Exn.to_string
  in
  let with_overlay x =
    let ol = overlay_location conf.template_location tmp_loc in
    Mount.with_mount ol x
  in
  let with_resources x =
    Mount.with_mounts conf.mountpoints x
  in
  create_locations () >>= fun _ ->
  with_overlay (fun () -> with_resources f) >>= fun res ->
  res
;;

end