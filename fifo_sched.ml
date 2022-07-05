open Printf
open Effect
open Effect.Deep
(* open Eio_domainslib_interface *)

exception Abort_take of string


module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val abort : unit -> unit
end

module Make () : S = struct

  type _ Effect.t += Fork  : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += Abort : unit Effect.t

  let run main =
    let run_q = Queue.create () in
    let enqueue t v =
      Queue.push (fun () -> continue t v) run_q
    in
    let rec dequeue () =
      if Queue.is_empty run_q then 
      (* raise (Abort_take "Queue empty") *)
      (* if !MVar.counter = 0 then *)
        begin
          printf "\nWe are waiting and run q is empty%!";
          Mutex.lock (MVar.m);
          while (!MVar.counter) <> 0 do
            Condition.wait (MVar.cv) (MVar.m)
          done;
          Mutex.unlock (MVar.m);
          perform Sched.Stuck
        end
      else 
      begin 

        Queue.pop run_q ()
      end
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
              let resumer v = 
                              if !MVar.sw then
                              begin
                                enqueue k v; 
                                true
                              end
                              else
                                false 
                        in
              if (f resumer) then
                 begin 
                 printf "\nTaking new task %!";dequeue ()
                 end
              else
                Printf.printf "\nFalse In Suspend%!"
               
                
                (* For now do nothing 
                TODO: Retry *)
              )
          | Sched.Stuck -> Some (fun (k: (a, _) continuation) ->
              Printf.printf "Reaching here in scheduler stuck";
              if Queue.is_empty run_q then
                try ignore (discontinue k Exit) with _ -> ()
              else begin
                enqueue k (); dequeue ()
              end)
           | Abort -> Some (fun (k: (a, _) continuation) ->
              (* Switch off the switch *)
              MVar.sw := false;

              Printf.printf "\nAborted in FIfo";
                    (* raise (Abort_take "Queue empty") *)
              (* Resume the calling fiber MAke it continue k () *)
              (* dequeue () *)
           )
          | e -> None }
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let abort () = perform Abort
end
