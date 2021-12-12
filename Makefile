.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

piece_test:
	OCAMLRUNPARAM=b dune exec test/piece_test.exe

board_test:
	OCAMLRUNPARAM=b dune exec test/board_test.exe

command_test:
	OCAMLRUNPARAM=b dune exec test/command_test.exe

state_test:
	OCAMLRUNPARAM=b dune exec test/state_test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	
docs:
	dune build @doc

zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst
