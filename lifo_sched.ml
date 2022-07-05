open Printf
open Effect
open Effect.Deep

exception Abort_take of string


module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val abort : unit-> unit

end

module Make () : S = struct

  type _ Effect.t += Fork  : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  type 'a cont = ('a,unit) continuation

  type _ Effect.t += Suspend : ('a cont -> unit) -> 'a Effect.t
  type _ Effect.t += Resume  : ('a cont * 'a) -> unit Effect.t
  type _ Effect.t += Abort : unit Effect.t

  let run main =
    let run_q = Stack.create () in
    let enqueue t v =
      Stack.push (fun () -> continue t v) run_q
    in
    let rec dequeue () =
      if Stack.is_empty run_q then perform Sched.Stuck
      else Stack.pop run_q ()
    in
    let rec spawn f =
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Yield -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); dequeue ())
          | Fork f -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); spawn f)
          | Sched.Suspend f -> Some (fun (k: (a,_) continuation) ->
              let resumer v =  if !MVar.sw then
                              begin
                                enqueue k v; 
                                true
                              end
                              else
                                false  in
              f resumer; dequeue ()) (*Dequeue only when f resumer is true*)
          | Sched.Stuck -> Some (fun (k: (a, _) continuation) ->
            Printf.printf "Reaching here in scheduler stuck";
              if Stack.is_empty run_q then
                ()
                (* try ignore (discontinue k Exit) with _ -> () *)
              else begin

                enqueue k (); dequeue ()
              end)
          | Abort -> Some (fun (k: (a, _) continuation) ->
               (* Switch off the switch *)
              MVar.sw := false;

              Printf.printf "\nAborted in LIfo";
              dequeue ()
              )            
          | e -> None }
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let abort () = perform Abort

end
