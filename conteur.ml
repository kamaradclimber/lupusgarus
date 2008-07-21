open Indi
open Definition

let c_nbjoueurs=8;;
let c_whoswho = Array.make c_nbjoueurs Unknown;;
let c_is_dead id=match c_whoswho.(id) with |Mort _->true|_->false;;
let joueurs=Array.init c_nbjoueurs (fun i-> new Joueur.joueur_de_base c_nbjoueurs i);;
let cimetiere=Array.make c_nbjoueurs (new Joueur.joueur_de_base c_nbjoueurs (-1));; (*symetrique  du tableau joueur qui contient les morts*)


let initialisation () (*ini du jeu: distribue les roles, demande a chaque joueur de s'initialiser en conséquence*) =
	let reparti = repartition c_nbjoueurs in 
	print_string "Repartition des joueurs:\n";
	print_perso_tab reparti;
	print_newline ();
	for id=0 to c_nbjoueurs-1 do 
		joueurs.(id)#donne_info (1,[|id;perso2int reparti.(id)|]);
		c_whoswho.(id)<- reparti.(id)
		done;
	for id=0 to c_nbjoueurs-1 do 
		let (_,reponse)=joueurs.(id)#pose_question (1,[|id|]) in
		Printf.printf "Arbitre: joueur %i  s'identifie comme %i etant un %s ce qui est %b\n" id reponse.(0) (perso2string ( int2perso reponse.(1))) (int2perso reponse.(1)= reparti.(id) && id=reponse.(0))
	done
;;

let the_end () (*gere la fin du jeu: affiche les gagants, le role de chacun...*)=
()
;;

let nuit () (*gere la nuit: ordre des perso à faire jouer, action de chacun*)=
()
;;

let jour () (*gere le jour: mort des personnages, action specifique, pendaison publique, election d'un maire.../*)=
	print_string "conteur: Le jour se leve...\n";
	print_string "conteur: procedons au vote\n";
	let vote=Array.make c_nbjoueurs 0 and tour=ref 1 and majorite=ref false and suspect=ref (-1) in
	while !tour <= 2 || (not !majorite) do
	(*majorité absolue au 1er tour ou relative au second    [ règle n°1] *)
		for id=0 to c_nbjoueurs-1 do
			if not (c_is_dead id) then (*corrigé: issue 5*)
				let (_,reponse) = joueurs.(id)#pose_question (2,[|!tour|]) in
				vote.(reponse.(0))<- vote.(reponse.(0)) + 1
			done;
		let (susp,maj) = vote_majorite vote in majorite:=maj ; suspect:=susp;
		incr tour
		done;
	Printf.printf "%i est donc pendu en place publique\n" !suspect;
	Printf.printf "il revele avant de monter sur l'echafaud qu'il était %s\n" (perso2string c_whoswho.(!suspect));
	for id=0 to c_nbjoueurs-1 do (*le conteur informe les joueurs de lexecution*)
		joueurs.(id)#donne_info (1,[|!suspect;perso2int c_whoswho.(!suspect)|]);
		joueurs.(id)#donne_info (3,[|!suspect;2|])
		done;
	c_whoswho.(!suspect)<- Mort c_whoswho.(!suspect); (*maj des infos du conteur*)
	cimetiere.(!suspect)<- joueurs.(!suspect);
;;


initialisation ();
jour ();


pause ();;

