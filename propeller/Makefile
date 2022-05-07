CC := /usr/bin/cc

.PHONY:all
all: propeller.native runtime

propeller.native: scanner.mll parser.mly ast.ml sast.ml semant.ml codegen.ml propeller.ml
	opam exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 propeller.native

.PHONY:clean
clean:
	ocamlbuild -clean
	cd runtime && make clean
	cd demos && make clean

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

.PHONY:runtime
runtime:
	cd runtime && make runtime

.PHONY:demos
demos: propeller.native runtime
	cd demos && make demos
