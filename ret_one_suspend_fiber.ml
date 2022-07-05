open Effect.Deep
open Printf
open Eio_linux
open Eio.Std



module F = Fifo_sched.Make ()

let m = MVar.create_empty ()
let main () =
  let comp () =
    
    Eio_linux.run (fun env ->
        Switch.run @@ fun sw ->
        ( Eio.Fiber.fork ~sw (fun _ ->
                Printf.printf "\nThread in Fifo scheduler taking into MVar\n";
                let v = MVar.take m in
                Printf.printf "\nThread in Fifo scheduler took %d into MVar\n" v;     
            );
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
