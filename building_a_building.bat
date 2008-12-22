ocamlc -c regles.ml
ocamlc -c unix.cma definition.ml
ocamlc -c joueur*.ml
ocamlc regles.ml unix.cma definition.ml joueur*.ml conteur.ml
pause