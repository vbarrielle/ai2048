
TARGETS=ai2048.native

all:
	ocamlbuild -use-ocamlfind -tag thread -pkg core $(TARGETS)

clean:
	ocamlbuild -clean

.DEFAULT_GOAL := all
.PHONY: all clean backups
