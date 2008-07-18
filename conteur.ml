let pause ()=
	Sys.catch_break true;
	Printf.printf "%s\n" "on fait la pause, ctrl+c pour reprendre";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend"
;;

pause ();;


let nombrejoueurs=2;;
open Joueur



let initialisation () (*ini du jeu:distribue les roles, demande a chaque joueur de sinitialiser*)=
Printf.printf "%i\n" (!Joueur.nbjoueurs);
Joueur.info (0,[|2;0|]);
Printf.printf "%i\n" (!Joueur.nbjoueurs);
;;

let the_end () (*gere la fin du jeu: affiche les gagants, le role de chacun...*)=
()
;;

let nuit () (*gere la nuit: ordre des perso à faire jouer, action de chacun*)=
()
;;

let jour () (*gere le jour: mort des personnages, action specifique, pendaison publique, election d'un maire.../*)=
()
;;


initialisation	();
	
	
	
pause ();;

