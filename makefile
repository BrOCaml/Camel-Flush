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

.PHONY: install
install:
	pip install -r requirements.txt
	opam install bisect_ppx ppx_inline_test ounit2 