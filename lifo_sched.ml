open Printf
open Effect
open Effect.Deep

exception Abort_take of string


module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val abort : (int MVar.t)-> unit

end

module Make () : S = struct

  type _ Effect.t += Fork  : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  type 'a cont = ('a,unit) continuation

  type _ Effect.t += Suspend : ('a cont -> unit) -> 'a Effect.t
  type _ Effect.t += Resume  : ('a cont * 'a) -> unit Effect.t
  type _ Effect.t += Abort : (int MVar.t) -> unit Effect.t

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
              let resumer v = enqueue k v in
              f resumer; dequeue ())
          | Sched.Stuck -> Some (fun (k: (a, _) continuation) ->
              if Stack.is_empty run_q then
                try ignore (discontinue k Exit) with _ -> ()
              else begin

                enqueue k (); dequeue ()
              end)
          | Abort mv -> Some (fun (k: (a, _) continuation) ->
              
              (* Set state  of cancellation context *)
              MVar.set_state mv;
              (* Execute cancellation function *)
              let _ = match Atomic.get (MVar.get_cancel_fn mv) with
              | None -> ()
              | Some fn -> fn () 
              in ();
              Printf.printf "\nAborted in LIfo";
              dequeue ()
                
               (* Execute cancel function *)
               (*let  _ = match Atomic.get (!MVar.cc).cancel_fn with
               | None -> ()
               | Some fn -> fn ()
                in (); *)
              (* Printf.printf "\nAborted in LIfo";
              try ignore (discontinue k (Abort_take "\nRunning in LIfo and aborting")) with 
               | Abort_take s -> Printf.printf "\n%s" s; dequeue ()
               | _ -> () *)
               
               )            
          | e -> None }
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let abort mv = perform (Abort mv)

end
