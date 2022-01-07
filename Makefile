all: display.js

#display.byte: rendu_etudiant.cmo html.cmo explore.cmo display.cmo
#	ocamlfind ocamlc -linkpkg -package js_of_ocaml-ppx -package js_of_ocaml html.cmo rendu_etudiant.cmo explore.cmo display.cmo -o display.byte

%.cmo: %.ml
	ocamlfind ocamlc -c -package js_of_ocaml-ppx -package js_of_ocaml $<

%.byte: %.ml
	ocamlbuild -use-ocamlfind -package js_of_ocaml -package js_of_ocaml-ppx $@



%.js: %.byte %.ml
	js_of_ocaml $<

with-dune:
	dune build -j 8 --profile release
	cp -f _build/default/display.bc.js display.js

clean:
	dune clean
	ocamlbuild -clean
	rm -f *.js
	rm *.cmo

.PHONY: all clean with-dune

display.ml: rendu_etudiant.ml
display.byte: rendu_etudiant.ml
display_ascii.ml: rendu_etudiant.ml

dist:
	mkdir projet
	cp rendu_etudiantVide.ml projet/rendu_etudiant.ml
	cp display.ml projet
	cp display.html projet
	cp display_ascii.ml projet
	cp html.ml projet
	cp Makefile projet
	cp fond-bois2.jpg projet
