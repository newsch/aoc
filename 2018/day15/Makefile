%.native: %.ml lib.ml
	ocamlbuild $@

all: day15.native day15p2.native


test: test_*.native
	for test in $? ; do ./$$test ; done

test_lib.native: test_lib.ml lib.ml
	ocamlbuild -use-ocamlfind -package ounit2 test_lib.native

.PHONY: all test
