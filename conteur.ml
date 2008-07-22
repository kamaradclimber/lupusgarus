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

let is_it_the_end () (*verifie si le jeu est terminé*)=
if not (array_exists (fun joueur -> joueur#get_id = (-1)) cimetiere) 
then (print_string "Conteur: Tout le monde est mort, le village de Salem s'est entretué !\n";true) (*tout le monde est mort*) 
else false (*temporaire*)
;;

let the_end () (*gere la fin du jeu: affiche les gagants, le role de chacun...*)=
()
;;

let nuit () (*gere la nuit: ordre des perso à faire jouer, action de chacun*)=
print_string "Conteur:La nuit tombe\n";
print_string "Conteur:les loups-garou se reveillent et rodent pendant la nuit\n";
let victime=Definition.appel_au_vote (fun id -> not (c_is_dead id) (*issue 5*) && (c_is_LG id)) (fun (idq,contenu)->c_is_dead (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 3 in
Stack.push victime morgue;
print_string "Conteur:Les loups-garous se rendorment\n"
		
;;

let jour () (*gere le jour: mort des personnages, action specifique, pendaison publique, election d'un maire.../*)=
	print_string "Conteur: Le jour se leve...\n";
	while not (Stack.is_empty morgue) do
		let id_mort = Stack.pop morgue in
		Printf.printf "Conteur: %i est mort cette nuit, %i etait %s\n" id_mort id_mort (perso2string c_whoswho.(id_mort));
		for id=0 to c_nbjoueurs-1 do (*le conteur informe les joueurs des morts*)
		joueurs.(id)#donne_info (1,[|id_mort;perso2int c_whoswho.(id_mort)|]);
		joueurs.(id)#donne_info (3,[|id_mort;1|])
		done;
		c_whoswho.(id_mort)<- Mort c_whoswho.(id_mort); (*maj des infos du conteur*)
		cimetiere.(id_mort)<- joueurs.(id_mort);
		done;
	print_string "Conteur: procedons au vote\n";
	let suspect=Definition.appel_au_vote (fun id -> not (c_is_dead id) ) (fun (idq,contenu)->c_is_dead (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 2 in
	Printf.printf "Conteur: %i est donc pendu en place publique\n" suspect;
	Printf.printf "Conteur: il revele avant de monter sur l'echafaud qu'il était %s\n" (perso2string c_whoswho.(suspect));
	for id=0 to c_nbjoueurs-1 do (*le conteur informe les joueurs de lexecution*)
		joueurs.(id)#donne_info (1,[|suspect;perso2int c_whoswho.(suspect)|]);
		joueurs.(id)#donne_info (3,[|suspect;2|])
		done;
	c_whoswho.(suspect)<- Mort c_whoswho.(suspect); (*maj des infos du conteur*)
	cimetiere.(suspect)<- joueurs.(suspect);
;;


initialisation ();
while not (is_it_the_end ()) do nuit ();jour ();flush stdout done;

flush stdout;
Sys.command "pause";;


