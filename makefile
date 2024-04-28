build:
	dune build

doc: 
	dune build @doc

.PHONY: test
test:
	OUNIT_CI=true dune test

clean:
	rm -rf _coverage
	dune clean