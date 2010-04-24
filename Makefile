
standard: clean
		ocamlopt unix.cmxa regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml joueur4.ml conteur.ml -o jouer

clean:
		rm -f *.cm* *.mli *.o *.html

ds: clean
		ocamlopt unix.cmxa regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml joueur4.ml ds_commun.ml -o ds_commun

doc: standard
		ocamldoc -d doc -html regles.ml
		ocamldoc -d doc -html definition.ml 
		ocamldoc -d doc -html joueur*.ml 
		ocamldoc -d doc -html conteur.ml
		make clean
