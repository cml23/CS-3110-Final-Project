.PHONY: test check
MAIN=main.byte

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec src/test

play:
	$(OCAMLBUILD) src/$(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f checkers.zip
	zip -r checkers.zip . -x@exclude.lst

clean:
	dune clean
	rm -f checkers.zip

doc:
	dune build @doc
