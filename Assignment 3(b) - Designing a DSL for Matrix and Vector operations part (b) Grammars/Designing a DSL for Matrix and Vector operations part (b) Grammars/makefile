OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

DSL_PARSER=mydsl

all: $(DSL_PARSER)
 
$(DSL_PARSER): ast.cmo parser.cmo lexer.cmo type_checker.cmo main.cmo
	$(OCAMLC) -o $@ $^
 
ast.cmo: ast.ml
	$(OCAMLC) -c $<
 
parser.ml parser.mli: parser.mly
	$(OCAMLYACC) $<
 
parser.cmi: parser.mli ast.cmo
	$(OCAMLC) -c $<
 
parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<
 
lexer.ml: lexer.mll
	$(OCAMLLEX) $<
 
lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) -c $< 
type_checker.cmo: type_checker.ml ast.cmo
	$(OCAMLC) -c $<
 
main.cmo: main.ml ast.cmo parser.cmi lexer.cmo type_checker.cmo
	$(OCAMLC) -c $<
 
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(DSL_PARSER)

.PHONY: all clean
