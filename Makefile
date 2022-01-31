compile:
	ocamlopt -o bdd utils.ml bdd.mli bdd.ml main.ml

info:
	@echo "utils.ml ----"
	@ocamlopt -i utils.ml
	@echo "\nbdd.ml ----"
	@ocamlopt -i bdd.ml
	@echo "\nbdd.mli ----"
	@ocamlopt -i bdd.mli
	@echo "\nmain.ml ----"
	@ocamlopt -i main.ml

depend:
	ocamldep options *.mli *.ml > .depend

run:
	./bdd