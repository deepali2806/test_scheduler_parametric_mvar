OCAMLPATH=~/repos/ocaml-multicore/_install/bin/

all:
	ocamlfind ocamlopt -o test_fifo.exe -thread fun_queue.mli fun_queue.ml sched.mli sched.ml MVar.mli MVar.ml \
		fifo_sched.mli fifo_sched.ml test_fifo.ml 

#all:
#	 ocamlfind ocamlopt -o test1.exe -linkpkg -package eio_linux -thread sched.mli sched.ml MVar.mli MVar.ml \
# 		fifo_sched.mli fifo_sched.ml lifo_sched.mli lifo_sched.ml test_fifo.ml 

# all:
# 	ocamlopt -o test.exe -g sched.mli sched.ml MVar.mli MVar.ml \
# 		fifo_sched.mli fifo_sched.ml lifo_sched.mli lifo_sched.ml test.ml


clean:
	rm -f *~ *.cm* *.o *.out *.exe
