open Effect

type 'a resumer = 'a -> bool
type _ Effect.t += Stuck : unit Effect.t
type _ Effect.t += Suspend : ('a resumer -> unit) -> 'a Effect.t
type _ Effect.t += Abort : unit Effect.t