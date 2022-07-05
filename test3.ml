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
                        if true then
                          raise Cancel
                        else
                        begin
                          let v = 52 in
                          Eio_domainslib_interface.MVar.put v m;
                        end
                        traceln "Eio Fiber 1 ends"

            ); 
            F.fork 
            ( fun _ -> 
              Printf.printf "\nReaching here inside fifo";
              printf "\nInside Second fibre ";
              let v = Eio_domainslib_interface.MVar.take m in
              Printf.printf "Take value %d" v
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

