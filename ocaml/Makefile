all:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex scanner.mll
	ocamlfind ocamlc -g -linkpkg \
		-package ppx_deriving.runtime -package ppx_deriving.show \
		code.ml graph.ml liveness.ml graphRegAlloc.ml genAmd64.ml \
		syntax.ml genCode.ml \
		parser.ml scanner.ml main.ml -o w
	OCAMLRUNPARAM=b ./w > a.s
	gcc -static a.s
	./a.out
clean:
	rm -rf *.cm* a.out a.s w parser.ml scanner.ml
