open Effect
(* open Printf *)

exception Abort_take of string
(* exception Race_condition *)

type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Fun_queue.t
  | Empty of 'a Sched.resumer Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty (Fun_queue.empty))

let create v = Atomic.make (Full (v, Fun_queue.empty))

let sw = ref true 


let counter = ref 0
let m = Mutex.create ()
let cv = Condition.create ()


(*Retry is remaining for effect handler*)
let rec put v mv =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> let p = ref true in 
                    perform (Sched.Suspend (fun r -> 
                                            let newQueue = Fun_queue.push q (v,r) in
                                            let new_contents = Full (v', newQueue) in
                                            p := Atomic.compare_and_set mv old_contents new_contents;
                                            counter := !counter + 1;
                                            !p
                                            )) 
                    (* if !check then
                      (printf "\nReaching here in Put check if";a)
                    else 
                      (printf "\nReaching here in Put check Else";put v mv) *)
  | Empty q ->
      if Fun_queue.length q = 0 then 
                  begin
                    let new_contents = Full (v, Fun_queue.empty) in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if (ret == false) then 
                      put v mv
                  end     
      else
          match Fun_queue.pop q with
                          | None -> ()
                          | Some (x, newQueue) -> let resume = x in
                                                  let new_contents = Empty newQueue in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                    counter := !counter - 1;
                                                    (if(!counter = 0) then
                                                      Condition.signal cv
                                                    else ());
                                                      let ret1 = resume v in
                                                      if ret1 then ()
                                                      else raise (Abort_take "Excception in Put because it is already aborted")  
                                                    end                                               
                                                  else
                                                    put v mv

        

let rec take mv =
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let p = ref true in 
                  perform (Sched.Suspend (fun r -> 
                                            Printf.printf "\nINside suspend%!";
                                            let newQueue = Fun_queue.push q r in
                                            let new_contents = Empty newQueue in
                                            p := Atomic.compare_and_set mv old_contents new_contents;
                                            counter := !counter + 1;
                                            Printf.printf "\nAfter suspend%!";
                                            !p
                                          )
                        ) 
                             
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    let ret = Atomic.compare_and_set mv old_contents new_contents in 
                    if ret then 
                      v
                    else 
                      take mv
                  end 
                else
                    match Fun_queue.pop q with
                    | None -> raise (Abort_take "Excception in take when queue popping from empty queue")
                    | Some ((v', resume), newQueue) -> 
                                                  let new_contents = Full (v', newQueue) in
                                                  let ret = Atomic.compare_and_set mv old_contents new_contents in 
                                                  if ret then
                                                    begin
                                                    counter := !counter - 1;
                                                    (
                                                      if(!counter = 0) then
                                                      Condition.signal cv
                                                      else ()
                                                    );

                                                    let ret1 = resume () in 
                                                      if ret1 then v
                                                      else raise (Abort_take "Excception in Take because it is already aborted")   
                                                    end
                                                  else
                                                    take mv               

