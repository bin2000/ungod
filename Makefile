UTIL=str.cma type.mli location.ml error.ml util.mli util.ml
SEMUTIL=type.mli util.ml
OCAMLC=ocamlc -warn-error A
TESTDIR=test

all: parser lexer semantics ungod
test: parser lexer test
run: all interactive

parser:
	ocamlyacc -v parser.mly
	${OCAMLC} -a -o parser.cma ${UTIL} parser.mli parser.ml

lexer:
	ocamllex lexer.mll
	${OCAMLC} -a -o lexer.cma ${UTIL} lexer.ml

semantics:
	${OCAMLC} -c ${SEMUTIL} semantics.ml

interactive:
	rlwrap ocaml ungod.cma

test: parser lexer
	make -C ${TESTDIR}


ungod:
	${OCAMLC} -a -o ungod.cma parser.cma lexer.cma semantics.cmo ungod.ml

clean:
	rm -f *.cmo *.cmi *.cmx *.o *.a *.spot *.spit *.annot *.cma *.cmxa lexer.ml parser.ml parser.mli parser.output
	make -C ${TESTDIR} clean
