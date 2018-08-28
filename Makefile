.PHONY: test build examples clean install uninstall reinstall

build:
	dune build @install

test:
	dune build @runtest

examples:
	dune build @examples

clean:
	dune clean

install: build
	dune install

uninstall: build
	dune uninstall

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

release:
	./opam-release.sh

doc:
	dune build @doc

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/_html/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@aws-s3'
	git -C .gh-pages commit -m "Update documentation"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages
