open Indi
open Definition

let c_nbjoueurs=8;;
let c_whoswho = Array.make c_nbjoueurs Unknown;;
let c_is_dead id=match c_whoswho.(id) with |Mort _->true|_->false;;
let c_is_LG id=match c_whoswho.(id) with |Loup->true|_->false;;
let joueurs=Array.init c_nbjoueurs (fun i-> new Joueur.joueur_de_base c_nbjoueurs i);;
let cimetiere=Array.make c_nbjoueurs (new Joueur.joueur_de_base c_nbjoueurs (-1));; (*symetrique  du tableau joueur qui contient les morts*)
let morgue=((Stack.create ()):int Stack.t);;


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
print_string "La nuit tombe\n";
print_string "les loups-garou se reveillent et rodent pendant la nuit\n";
let vote=Array.make c_nbjoueurs 0 and tour=ref 1 and majorite=ref false and victime=ref (-1) in
	while !tour <= 2 && (not !majorite) do
	(*majorité absolue au 1er tour ou relative au second    [ règle n°1] *)
		for id=0 to c_nbjoueurs-1 do
			if not (c_is_dead id) (*corrigé: issue 5*) && (c_is_LG id) then
				let reponse=ref (snd (joueurs.(id)#pose_question (3,[|!tour|]))) and nbessais=ref 1 in
				while c_is_dead (!reponse).(0) && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
					Printf.printf "Arbitre: %i vote contre un mort, il n'a plus que %i essais avant de voter contre lui meme\n" id (Regles.nb_vote_max- !nbessais);
					reponse := snd (joueurs.(id)#pose_question (3,[|!tour|]));
					incr nbessais
					done;
				if !nbessais = Regles.nb_vote_max (*vote contre lui meme [regle n°3] *)
					then (vote.(id)<-vote.(id)+1;Printf.printf "Arbitre: %i vote contre lui meme car il a dépassé la barre des %i votes incorrects\n" id Regles.nb_vote_max)
					else (vote.((!reponse).(0))<- vote.((!reponse).(0)) + 1 ;Printf.printf "Arbitre: %i (LG) vote contre %i\n" id (!reponse).(0))
			done;
		let (vict,maj) = vote_majorite vote in majorite:=maj ; victime:=vict;
		Printf.printf "Arbitre: majorité: %b, tour: %i\n" !majorite !tour;
		incr tour
		done;
		Stack.push !victime morgue
;;

let jour () (*gere le jour: mort des personnages, action specifique, pendaison publique, election d'un maire.../*)=
	print_string "conteur: Le jour se leve...\n";
	print_string "conteur: procedons au vote\n";
	let vote=Array.make c_nbjoueurs 0 and tour=ref 1 and majorite=ref false and suspect=ref (-1) in
	while !tour <= 2 && (not !majorite) do
	(*majorité absolue au 1er tour ou relative au second    [ règle n°1] *)
		for id=0 to c_nbjoueurs-1 do vote.(id)<- 0 done; (*remise a zero des votes*)
		for id=0 to c_nbjoueurs-1 do
			if not (c_is_dead id) then (*corrigé: issue 5*)
				let reponse=ref (snd (joueurs.(id)#pose_question (2,[|!tour|]))) and nbessais=ref 1 in
				while c_is_dead (!reponse).(0) && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
					Printf.printf "Arbitre: %i vote contre un mort, il n'a plus que %i essais avant de voter contre lui meme\n" id (Regles.nb_vote_max- !nbessais);
					reponse := snd (joueurs.(id)#pose_question (2,[|!tour|]));
					incr nbessais
					done;
				if !nbessais = Regles.nb_vote_max (*vote contre lui meme [regle n°3] *)
					then (vote.(id)<-vote.(id)+1;Printf.printf "Arbitre: %i vote contre lui meme car il a dépassé la barre des %i votes incorrects\n" id Regles.nb_vote_max)
					else (vote.((!reponse).(0))<- vote.((!reponse).(0)) + 1 ;Printf.printf "Arbitre: %i vote contre %i\n" id (!reponse).(0))
			done;
		let (susp,maj) = vote_majorite vote in majorite:=maj ; suspect:=susp;
		Printf.printf "Arbitre: majorité: %b, tour: %i\n" !majorite !tour;
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
for i=0 to 3 do nuit ();jour () done;

Sys.command "pause";;

