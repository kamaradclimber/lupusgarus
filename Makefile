
standard: clean
		ocamlc unix.cma regles.ml definition.ml joueur*.ml conteur.ml -o jouer

clean:
		rm -f *.cm* *.mli *.o *.html

ds: clean
		ocamlc unix.cma regles.ml definition.ml joueur*.ml ds_commun.ml -o ds_commun

doc: clean
		ocamldoc -d doc -html *.ml
