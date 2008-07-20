let pause ()=
	Sys.catch_break true;
	Printf.printf "%s\n" "on fait la pause, ctrl+c pour reprendre";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend"
;;

pause ();;

open Definition
open Joueur (*nest-ce pas une faille de louvrir ??*)

let c_nbjoueurs=3;;


let initialisation () (*ini du jeu:distribue les roles, demande a chaque joueur de sinitialiser*)=
let j1=new joueur_de_base c_nbjoueurs 0 and j2=new joueur_de_base c_nbjoueurs 1 in
()
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

