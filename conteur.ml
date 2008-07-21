let pause ()=
	Sys.catch_break true;
	Printf.printf "%s\n" "on fait la pause, ctrl+c pour reprendre";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend"
;;

open Definition
open Joueur (*nest-ce pas une faille de louvrir ??*)

let c_nbjoueurs=3;;
let c_whoswho = Array.make c_nbjoueurs Unknown;;


let initialisation () (*ini du jeu:distribue les roles, demande a chaque joueur de sinitialiser en conséquence*) =
let joueurs=Array.init c_nbjoueurs (fun i-> new joueur_de_base c_nbjoueurs i) in
let reparti = repartition c_nbjoueurs in 
print_string "Repartition des joueurs:\n";
print_perso_tab reparti;
for id=0 to c_nbjoueurs-1 do joueurs.(id)#donne_info (1,[|id;perso2int reparti.(id)|]) done;
for id=0 to c_nbjoueurs-1 do 
	let (_,reponse)=joueurs.(id)#pose_question (1,[|id|]) in
	Printf.printf "joueur %i " id;
	Printf.printf "s'identifie comme %i étant un %s ce qui est %b\n" reponse.(0) (perso2string ( int2perso reponse.(1))) (int2perso reponse.(1)= reparti.(id) && id=reponse.(0))
	done
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

