
standard: clean
		ocamlc unix.cma regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml conteur.ml -o jouer

clean:
		rm -f *.cm* *.mli *.o

ds: clean
		ocamlc unix.cma regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml ds_commun.ml -o ds_commun
