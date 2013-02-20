(*
Utility functions for use elsewhere.
*)

open Core.Std;;
open Unix;;

module Pipe_infix = struct
  let (>|) v f = f v;;
  let (<|) f v = f v;;
end

module Rich_list = struct
  let partial_zip l1 l2 =
    let open List in
    let rec pzip_helper l1 l2 acc = match (l1, l2) with
    | (a :: t1, b :: t2) -> pzip_helper t1 t2 ((a,b) :: acc)
    | _ -> acc in
    rev (pzip_helper l1 l2 [])
  ;;

  include List
end

module List = Rich_list

(*
Executes a process, waits until it finishes, and then returns the exit status.
*)
let exec_wait command args =
  let proc = create_process command args in
  let pid = proc.Process_info.pid in
  waitpid pid
;;