EXEC = fbtoc

CAMLC = ocamlc
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

all:
	$(CAMLLEX) scanner.mll
	$(CAMLYACC) parser.mly
	$(CAMLC) -c parser.mli
	$(CAMLC) -c scanner.ml
	$(CAMLC) -c parser.ml
	$(CAMLC) scanner.cmo parser.cmo freebasic.ml -o $(EXEC)

clean:
	rm -f *.cm[iox] *.mli *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt
	rm -rf scanner.ml parser.ml
