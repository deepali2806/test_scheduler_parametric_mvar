type 'a t
type state
type context
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> unit
val take : int t -> int
val set_state : 'a t -> unit
val get_cancel_fn : 'a t -> (unit Sched.resumer) option Atomic.t
(* val cc : context ref
val get_cc : unit -> context ref
val set_state_cancel : unit
val get_cancel_fn : (unit Sched.resumer) option Atomic.t *)
(* val p : int ref *)
(* val abort : 'a t -> 'a -> unit *)
