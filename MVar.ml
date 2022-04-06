open Effect

exception Abort_take of string

type state =
  | On 
  | Cancelling 
  (* | Finished *)

type context = {
    mutable state : state;
    cancel_fn : (unit Sched.resumer) option Atomic.t;
}

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Queue.t
  | Empty of 'a Sched.resumer Queue.t

type 'a mv_cc = {
  mv_st : 'a mv_state;
  cc : context
}

type 'a t = 'a mv_cc ref

let create_empty () = ref {mv_st = Empty (Queue.create ()); cc = { state = On ;cancel_fn = Atomic.make None} }

let create v = ref {mv_st = (Full (v, Queue.create ())); cc = { state = On ;cancel_fn = Atomic.make None} }

(* let create_context () = { state = On ;cancel_fn = Atomic.make None} *)

let set_cancel_fn t fn =
    (* if Atomic.exchange t.cancel_fn (Some fn) <> None then failwith "Fiber already has a cancel function!" *)
    Atomic.set t.cancel_fn (Some fn)

let set_state mv = 
            let _ = 
            begin match !mv with
            | {mv_st; cc} -> begin match cc with
                            | {state; cancel_fn} -> mv := { mv_st; cc = {state = Cancelling; cancel_fn} } end
            end
            in ()

let get_cancel_fn mv = 
          begin match !mv with
            | {mv_st; cc} -> begin match cc with
                            | {state; cancel_fn} -> cancel_fn end
            end

let put v mv =
  match (!mv).cc.state with
  | On -> (match (!mv).mv_st with
          | Full (v', q) -> Printf.printf "Suspend in put"; 
                            perform (Sched.Suspend (fun r -> 
                            Atomic.set (!mv).cc.cancel_fn (Some (fun () -> Printf.printf "\nCancel function is invoked so restoring continuation";
                                                              let (v', resume) = Queue.pop q in resume ()
                                                              ));
                            Queue.push (v,r) q))
          | Empty q ->
              if Queue.is_empty q then
                mv := {mv_st = Full (v, Queue.create ()); cc = (!mv).cc }
              else
                let resume = Queue.pop q in
                resume v)
  | Cancelling -> Printf.printf "Cancelled"; raise (Abort_take "Excception in Put because it is already aborted")

  



let take mv =
  (* Check if state is cancelled? *)
  match (!mv).cc.state with
  | On -> (match (!mv).mv_st with
            | Empty q -> Printf.printf "Suspend in take"; perform (Sched.Suspend (fun r -> 
                (* let fn = Queue.pop q in  *)
                  Atomic.set (!mv).cc.cancel_fn (Some (fun () -> Printf.printf "\nCancel function is invoked so restoring continuation";
                                                              let v = 10 in
                                                              let resume = Queue.pop q in resume v
                                                              (* let resume = Queue.pop q in resume () ;() *)
                                                              ));
                Queue.push r q
              )
            )
            | Full (v, q) ->
                if Queue.is_empty q then
                  (mv := { mv_st = Empty (Queue.create ()); cc = (!mv).cc }; v)
                else begin
                  let (v', resume) = Queue.pop q in
                  mv := {mv_st = Full (v', q); cc = (!mv).cc};
                  resume ();
                  v
                end
          )
  | Cancelling -> Printf.printf "Cancelled"; raise (Abort_take "Excception in Take because it is already aborted")
    
              