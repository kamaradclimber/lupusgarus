
standard: clean
		for f in *.ml; do mv $$f $$f.bak; cat $$f.bak | sed -e 's/\\130/é/' | sed -e 's/\\138/è/' | sed -e 's/\\133/à/' | sed -e 's/\\136/ê/' > $$f; done
		ocamlc unix.cma regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml conteur.ml -o jouer
		for f in *.ml; do mv -T $$f.bak $$f; done 

clean:
		rm -f *.cm* *.mli *.o *.html

ds: clean
		ocamlc unix.cma regles.ml definition.ml joueur.ml joueur2.ml joueur3.ml ds_commun.ml -o ds_commun

doc: clean
		ocamldoc -d doc -html regles.ml
		ocamldoc -d doc -html definition.ml 
		ocamldoc -d doc -html joueur*.ml 
		ocamldoc -d doc -html conteur.ml