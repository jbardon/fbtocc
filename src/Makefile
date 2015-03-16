EXEC = fbtocc

CAMLC = ocamlc
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

all:
	$(CAMLLEX) scanner.mll
	$(CAMLYACC) -v parser.mly
	$(CAMLC) -c expression.ml		
	$(CAMLC) -c error.ml	
	$(CAMLC) -c definitions.ml		
	$(CAMLC) -c parser.mli
	$(CAMLC) -c scanner.ml
	$(CAMLC) -c parser.ml
	$(CAMLC) expression.cml error.cmo definitions.cmo scanner.cmo parser.cmo freebasic.ml -o $(EXEC)
	make clean

clean:
	rm -f *.cm[iox] *.mli *~ .*~ #*#
	rm -f output.c #$(EXEC)# 
	rm -f $(EXEC).opt
	rm -rf scanner.ml parser.ml
