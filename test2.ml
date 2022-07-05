open Effect
open Effect.Deep

open Printf  
open Eio_linux
open Eio.Std

exception Abort_take of string


module F = Fifo_sched.Make ()

exception Cancel

let m = Eio_domainslib_interface.MVar.create_empty ();;

let main () =
  let comp () =

    Eio_linux.run @@ fun _env ->
    F.run (fun () ->  
    Switch.run @@ fun sw ->
    (        
            Eio.Fiber.fork ~sw (
                fun () ->
                        traceln "\nEio Fiber 1 started ";
                        Fiber.yield();
                        let v = 52 in
                        Eio_domainslib_interface.MVar.put v m;
                        traceln "Eio Fiber 1 ends"
            ); 
            F.fork 
            ( fun _ -> 
              Printf.printf "\nReaching here inside fifo";
              (try 
                Eio.Fiber.fork ~sw (
                  fun () -> printf "\nCancelling fiber :Reached here"; raise Cancel
                )
              with Cancel -> printf "\nHopefully other thread also cancelled");

              printf "\nInside Second fibre ";
              let v = Eio_domainslib_interface.MVar.take m in
              Printf.printf "\nTake value %d" v
            )      
            
        )

    )
    in
      match_with comp ()
      { retc = (fun () -> ());
        exnc = (function
          | Exit ->  Printf.printf "\nReached at Exit exception";()
          | e -> Printf.printf "\nReaching here in exception\n";
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) 
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Sched.Stuck -> Some (fun (k : (a,_) continuation) -> Printf.printf "\nReached at stuck";
              discontinue k Exit
              )
          | e -> None }

let _ = main ()

(* 
let flip = let () = Random.self_init () in Random.bool

let test_abort () =
  printf "\nTesting potential abort";
  let did_close = ref false in
  let sub () =
    let finally () = did_close := true in
    Fun.protect ~finally @@ fun () ->
    Fiber.yield ();
    printf "\nFiber finished";
  in
  let main () =
    let f = Fiber.spawn sub in
    Fiber.yield ();
    if flip () then (printf " Aborting fiber!"; Fiber.abort f)
  in
  ignore (Fiber.run main);
  assert (!did_close) *)
