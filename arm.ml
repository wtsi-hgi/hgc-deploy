(*
  Automatic resource management for OCaml.
  *)
open Core.Std

module type Resource = sig

  type t

  val open_r : t -> unit

  val close_r : t -> unit
  
end

module ManagedResource(M : Resource) : sig

  type resource = M.t

  (* Takes a resource and produces a value of type 'a *)
  type 'a t = resource -> 'a

  (* Run the monad in the context of the resource. *)
  val run : rsc:resource -> f:('a t)-> (('a, exn list) Result.t)

  include Monad.S with type 'a t := 'a t

end = struct 

  module T = struct

    type resource = M.t

    (* Takes a resource and produces a value of type 'a *)
    type 'a t = resource -> 'a

    let run ~rsc ~f : ('a, exn list) Result.t = 
      try
        M.open_r rsc;
        let result = f rsc in
        M.close_r rsc;
        Ok result
      with
      | e -> begin
        M.close_r rsc;
        Error [e]
      end
    ;;

    let return x = (fun _ -> x)

    let bind m f = (fun rsc -> (f (m rsc)) rsc)

  end

  include T
  include Monad.Make(T)

end

