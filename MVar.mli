type 'a t
exception Abort_take of string
(* exception Race_condition *)

(* val sw : bool ref *)
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> unit
val take : 'a t -> 'a

