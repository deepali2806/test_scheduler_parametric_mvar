module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val abort : (int MVar.t) -> unit

end

module Make () : S
