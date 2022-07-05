open Printf
open Effect
open Effect.Deep

let global_id = ref 0

module type S = sig
  type state = TRANS | CANCEL
  type fiber = {id : int ; 
                st : state ref }
  val fork : fiber -> (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val make_fiber : unit -> fiber
end

module Make () : S = struct
type state = TRANS | CANCEL
  type fiber = {id : int ; 
                st : state ref }

  type _ Effect.t += Fork  : (fiber * (unit -> unit)) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t


  let make_fiber () = let new_id = !global_id + 1 in
                  global_id := new_id;  
                {id = !global_id; st = ref TRANS }

  let run main =
    let run_q = Queue.create () in
    let enqueue (t, fiber) v =
      Queue.push (fun () -> continue t v) run_q
    in
    let rec dequeue () =
      if Queue.is_empty run_q then perform Sched.Stuck
      else 
      begin 
      match Queue.pop run_q () with
     
      end
    in
    let rec spawn fiber fn =
      match_with fn ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Yield -> Some (fun (k: (a,_) continuation) ->
              enqueue (k, fiber) (); dequeue ())
          | Fork (newfiber,f) -> Some (fun (k: (a,_) continuation) ->
              enqueue (k, fiber) (); 
              spawn newfiber f
              )
          | Suspend f -> Some (fun (k: (a,_) continuation) ->
               
                f (function 
                    Ok v ->  enqueue (k, fiber) v
                    Error ex ->  enqueue (k, fiber) ex
                  ); 
                dequeue ();
              )
          | e -> None }
    in
    spawn (make_fiber ()) main

  let fork new_fiber f = perform (Fork (new_fiber,f))
  let yield () = perform Yield
end
