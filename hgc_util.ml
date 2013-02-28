(*
Utility functions for use elsewhere.
*)

open Core.Std;;
open Unix;;

module Pipe_infix = struct
  let (|>) v f = f v;;
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

module Shell = struct

  module Result = struct
    type t = {
      status : Exit_or_signal.t;
      stdout : string;
      stderr: string
    }
  end

  let run command args =
    let proc = create_process command args in
    let pid = proc.Process_info.pid in
    let out = In_channel.input_all (in_channel_of_descr proc.Process_info.stdout) in
    let err = In_channel.input_all (in_channel_of_descr proc.Process_info.stderr) in
    Result.({status = (waitpid pid); stdout = out; stderr = err})
  ;;

  let exec_wait command args = 
    (run command args).Result.status 
  ;;

  let fork_exec command args =
    waitpid (fork_exec command ("command"::args) ())
  ;;

end