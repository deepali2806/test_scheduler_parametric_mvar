open Effect
open Effect.Deep
open Printf

module F = Fifo_sched.Make ()
module L = Lifo_sched.Make ()

let m = MVar.create_empty ()
(* let m = MVar.create 25 *)
let main () =
  let comp () =
    
    F.run (fun () ->
    L.run (fun () ->
      F.fork (fun _ ->
          Printf.printf "\nThread in Fifo scheduler taking into MVar\n";
          (* F.abort m; *)
          let v = MVar.take m in
          Printf.printf "\nThread in Fifo scheduler took %d into MVar\n" v;     

          (* Printf.printf "\nThread in Fifo scheduler putting in MVar\n";
          F.abort m;
          let v = 42 in
          MVar.put v m;
          Printf.printf "\nThread in Fifo scheduler put %d in MVar\n" v *)

        );

      L.fork (fun _ ->
          Printf.printf "\nThread in Lifo scheduler putting in MVar\n";
          (try F.abort () with
            | _ -> printf "Something happened here"
          );
          printf "Reaching here in Lifo";
          let v = 42 in
          try MVar.put v m with
          _ -> Printf.printf "Catching exception";
          Printf.printf "\nThread in Lifo scheduler put %d in MVar\n" v
          (* Printf.printf "\nThread in Lifo scheduler taking into MVar\n";
          let v = MVar.take m in
          Printf.printf "\nThread in Lifo scheduler took %d into MVar\n" v;    *)
      
        )
      )
    )
  in
  match_with comp ()
  { retc = (fun () -> ());
    exnc = (function
      | Exit ->  Printf.printf "\nReached at Exit exception";()
      | e -> Printf.printf "\nReaching here"; Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Sched.Stuck -> Some (fun (k : (a,_) continuation) -> Printf.printf "\nReached at stuck";
          (* discontinue k Exit *)
          ()
          )
      | e -> None }

let _ = main ()
