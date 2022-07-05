open Effect.Deep
open Printf

module F = Fifo_sched.Make ()

let m = MVar.create_empty ()
let main () =
  let comp () =

    F.run (fun () ->

      F.fork (fun _ ->
            Printf.printf "\nFifo Fiber 1\n%!";
            let v = MVar.take m in
            Printf.printf "\nFifo Fiber 1 ends %d\n%!" v;
      );

      F.fork (fun _ ->
            Printf.printf "\nFifo Fiber 3\n%!";
            let v = MVar.take m in
            Printf.printf "\nFifo Fiber 3 ends %d\n%!" v;
      );

     (* F.fork (fun _ ->
            Printf.printf "\nFifo fiber 2\n%!";
            let v = 42 in 
            MVar.put v m;
            Printf.printf "\nFifo Fiber 2 ends\n%!";
      ); *)

       (* F.fork (fun _ ->
            Printf.printf "\nFifo fiber 4\n";
            let v = 44 in 
            MVar.put v m;
            Printf.printf "\nFifo Fiber 4 ends\n";
      ); *)
    )   
  in
  match_with comp ()
  { retc = (fun () -> ());
    exnc = (function
      | Exit ->  Printf.printf "\nReached at Exit exception";()
      | e -> Printf.printf "\nReaching here in exception"; Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | Sched.Stuck -> Some (fun (k : (a,_) continuation) -> Printf.printf "\nReached at stuck";
          (* discontinue k Exit *)
          ()
          )
      | e -> None }

let _ = main ()
