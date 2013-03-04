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

  let mk_string lst ?(first = "") ?(last = "") ~sep ~f = 
    let flst = List.map lst ~f in 
    match (List.reduce flst ~f:(fun a b -> (a ^ sep ^ b))) with
    | Some a -> (first ^ a ^ last)
    | None -> first ^ last
  ;;

  include List
end

module List = Rich_list

module Shell = struct

  let clean_env = [
    ("SHELL", "/bin/bash");
    ("PATH", "/usr/local/bin:/usr/bin:/bin");
    ("LANG", "en_GB.UTF-8");
  ]

  module Result = struct
    type t = {
      status : Exit_or_signal.t;
      stdout : string;
      stderr: string
    }
  end

  let run command args =
    let proc = create_process_env ~prog:command ~args ~env:(`Replace clean_env) () in
    let pid = proc.Process_info.pid in
    let out = In_channel.input_all (in_channel_of_descr proc.Process_info.stdout) in
    let err = In_channel.input_all (in_channel_of_descr proc.Process_info.stderr) in
    Result.({status = (waitpid pid); stdout = out; stderr = err})
  ;;

  let fork_wait command args =
    let env = List.map clean_env (function | (a,b) -> a ^"="^ b) in
    waitpid (fork_exec ~prog:command ~args:(command::args) ~env ())
  ;;

end