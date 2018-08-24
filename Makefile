.PHONY: test build examples clean

build:
	dune build @install

test:
	dune build @runtest

examples:
	dune build @examples

clean:
	dune clean
