CC := /usr/bin/cc

.PHONY:all
all: propeller.native

propeller.native: scanner.mll parser.mly ast.ml sast.ml semant.ml codegen.ml propeller.ml
	opam exec -- \
	ocamlbuild -use-ocamlfind propeller.native

.PHONY:clean
clean:
	ocamlbuild -clean
