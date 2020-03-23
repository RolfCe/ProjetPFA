all : main
	./main

main : main.cmo
	ocamlc -o main graphics.cma main.cmo

Button.cmi : Button.mli
	ocamlc -c Button.mli

menu.cmi : menu.mli
	ocamlc -c menu.mli

main.cmo : menu.cmi Button.cmi main.ml
	ocamlc -c main.ml

