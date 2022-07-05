open Effect
open Effect.Deep
open Printf
open Eio_linux
open Eio.Std

exception Cancel

(* module F = New_fifo_sched.Make () *)
(* module L = Lifo_sched.Make () *)

(* let m = MVar.create_empty () *)
(* let m = MVar.create 25 *)
let main () =
  let comp () =

    Eio_linux.run @@ fun _env ->
    Switch.run @@ fun sw ->
    (        
            try Eio.Fiber.fork ~sw (
                fun () ->
                        traceln "\nEio Fiber 1 started ";
                        Fiber.yield();
                        (* let v = 52 in
                        Eio_domainslib_interface.MVar.put v m; *)
                        traceln "Eio Fiber 1 ends"
            ); 
            Eio.Fiber.fork ~sw (
                fun () ->
                        traceln "\nEio Fiber 2 started ";
                        raise Cancel
                        (* Fiber.yield();
                        let v = 52 in
                        Eio_domainslib_interface.MVar.put v m; *)
                        traceln "Eio Fiber 2 ends"
            ) with
            Cancel -> traceln "Just checking if exception is catched in Cancel?"
    )
    

    (* F.run (fun () ->
        let fiber1 = F.make_fiber () in 
        F.fork fiber1 ( fun () ->
                    F.yield ();
                    printf "Inside Fiber 1";
        );

        let fiber2 = F.make_fiber () in 
        F.fork fiber2 ( fun () ->
            printf "Inside Fiber 2";
        );
    ) *)

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
