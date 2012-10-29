OCAML_FIND ?= ocamlfind

build:
	ocamlbuild -use-ocamlfind test.byte test.native camltc.cma camltc.cmxa camltc.a

clean:
	ocamlbuild -use-ocamlfind -clean

install:
	$(OCAML_FIND) install camltc META \
	  _build/camltc.cma \
	  _build/camltc.cmxa \
	  _build/camltc.cmi \
	  _build/camltc.mli \
          _build/libotc.a \
	  _build/camltc.a \
          _build/3rd-party/tokyocabinet-1.4.47/libtokyocabinet.a 


uninstall:
	$(OCAML_FIND) remove camltc


default: build

