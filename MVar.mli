type 'a t
exception Abort_take of string
val sw : bool ref
val create : 'a -> 'a t
val create_empty : unit -> 'a t
val put : 'a -> 'a t -> unit
val take : 'a t -> 'a
val counter : int ref
val m : Mutex.t
val cv : Condition.t
