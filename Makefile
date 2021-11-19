compile:
	ocamlopt -o bdd utils.ml bdd.ml main.ml

info:
	@echo "utils.ml ----"
	@ocamlopt -i utils.ml
	@echo "\nbdd.ml ----"
	@ocamlopt -i bdd.ml
	@echo "\nmain.ml ----"
	@ocamlopt -i main.ml