CC := /usr/bin/cc

.PHONY:all
all: propeller.native

propeller.native: scanner.mll parser.mly ast.ml sast.ml semant.ml codegen.ml propeller.ml
	opam exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 propeller.native

.PHONY:clean
clean:
	ocamlbuild -clean

.PHONY:test
test: test_parser test_hello test_extended

.PHONY:test_parser
test_parser: propeller.native
	tests/tests-parser.sh

.PHONY:test_hello
test_hello: propeller.native
	tests/tests-hello.sh

.PHONY:test_extended
test_extended: propeller.native
	tests/tests-extended.sh
