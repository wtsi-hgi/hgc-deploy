(*
  Deal with GPG functionality for verifying signatures.
*)

open Core.Std;;
open Sys;;
open Unix;;

(*
  Verify that the file is signed by a suitable trusted signature.
*)
let verify signature file = 
  let args = ["--verify"; signature; file] in
  let gpg = create_process "gpg" args in
  let pid = gpg.Process_info.pid in
  match waitpid pid with
  | Ok () -> true
  | _ -> false
;;