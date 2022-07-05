module type S = sig
  type state = TRANS | CANCEL
  type fiber = {id : int ; 
                st : state ref } 
  val fork : fiber -> (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val make_fiber : unit -> fiber
end

module Make () : S