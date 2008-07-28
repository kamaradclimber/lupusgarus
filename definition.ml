let verbose=3;;

Random.init (int_of_float (Unix.time ()));;

let v_print_string level str=if verbose<= level then print_string str;;

let print_int_tab tab= Array.iter (fun x->Printf.printf "%i " x) tab;print_newline ();;

let array_exists predicat tab=
	let i=ref 0 in
	while !i<Array.length tab && not (predicat tab.(!i)) do incr i done;
	not (!i=Array.length tab)
;;

let array_all predicat tab= not (array_exists (fun x->not (predicat x)) tab);;

(*contient les definitions de types et les parametres fondamentaux*)

type perso = Unknown |Loup | Villageois| Voyante| Sorciere|Mort of perso
type information=int*(int array);;

let int2perso n=
	match n with |0->Unknown |1->Loup|2->Villageois|3->Voyante |4->Sorciere|_->assert false
;;
let rec perso2int pers=
	match pers with 
		|Unknown -> 0 |Loup->1|Villageois->2 |Voyante->3|Sorciere->4
		|Mort sthing ->(v_print_string 4 "vous avez demandé l'identification perso2int d'un mort ATTENTION";print_newline ();perso2int sthing)
;;
let rec perso2string pers=
	match pers with |Unknown -> "Unknown" |Loup->"Loup"|Villageois->"Villageois"|Voyante->"Voyante"|Sorciere->"Sorciere"|Mort persbis-> (perso2string persbis)^" (Mort)"
;;
let print_perso_tab tab=
	Array.iter (fun x->v_print_string 2 (Printf.sprintf "%s " (perso2string x)) ) tab;print_newline ()
;;

let perso_is_dead  pers=match pers with Mort _->true |_->false;;

(*classe des joueurs: dans chaque module, le joueur definit sa sous classe avec sa manière propre de repondre aux questions et dassimiler les informations*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    val nbjoueurs = c_nbjoueurs
	val id = numjoueur
	val virtual classe : string
	method virtual donne_info : information -> unit
	method virtual pose_question : information -> information
	method get_id = (id:int)
	method get_nbjoueurs = (nbjoueurs:int)
  end;;
 
let repartition nbjoueurs=
	let rep=Array.init nbjoueurs (fun i->i) in
	let perm tab i1 i2= let tmp=tab.(i1) in tab.(i1)<-tab.(i2); tab.(i2)<-tmp in
	for i=0 to 2*nbjoueurs do perm rep (Random.int nbjoueurs) (Random.int nbjoueurs) done;
	(*on a desormais un tableau ãléatoire avec les id des joueurs*)
	let rep2=Array.make nbjoueurs Villageois in
	for i=0 to nbjoueurs-1 do 
		match i mod 3 with
			|0->rep2.(i)<- Loup
			|_->()
		done;
	rep2.(1)<- Voyante (*probleme: s'il n'ya qu'un joueur*);
	rep2.(2)<- Sorciere (*probleme: s'il n'ya que deux joueurs*);
	let rep3=Array.make nbjoueurs Unknown in
	for i=0 to nbjoueurs-1 do rep3.(rep.(i))<-rep2.(i) done;
	rep3
;;

let vote_majorite (resultats:int array)=
	let imaxl=ref [0] and sum=ref 0 in
	for i=1 to Array.length resultats-1 do (*le fait de commencer à 1 causera un bug s'il n'y a qun joueur*)
		sum:=!sum+ resultats.(i);
		match compare resultats.(List.hd !imaxl) resultats.(i) with
			|0-> imaxl := (i::(!imaxl))
			|(-1)->imaxl:=[i]
			|_-> ()
		done;
	let tmp=Array.of_list !imaxl in let n=Array.length tmp in 
	let choix=tmp.(Random.int n) (*si égalité, le hasard décide [règle n°2 ] +  correction issue8 *) in
	(choix, resultats.(choix) > (!sum)/2) (*joueur plébiscité, majorité absolue*)
;;

let communication_du_vote (condition_de_vote: (int ->bool)) c_nbjoueurs joueurs info_a_transmettre=
for id=0 to c_nbjoueurs-1 do if condition_de_vote id then joueurs.(id)#donne_info info_a_transmettre done
;;

let appel_au_vote (condition_de_vote: (int ->bool)) (vote_invalide:information->bool) c_nbjoueurs joueurs idq id_vote type_vote=
	let vote=Array.make c_nbjoueurs 0 and tour=ref 1 and majorite=ref false and victime=ref (-1) and nb_votants = ref 0 in
	while !tour <= 2 && (not !majorite) do
	(*majorité absolue au 1er tour ou relative au second    [ règle n°1] *)
		for id=0 to c_nbjoueurs-1 do vote.(id)<- 0 done; (*remise a zero des votes*)
		for id=0 to c_nbjoueurs-1 do
			if condition_de_vote id then
				begin
				incr nb_votants;
				let reponse=ref (joueurs.(id)#pose_question (idq,[|!tour|])) and nbessais=ref 1 in
				while vote_invalide !reponse && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
					v_print_string 2 (Printf.sprintf "Arbitre: %i vote contre un mort (%i), il n'a plus que %i essais avant de voter contre lui meme\n" id ((snd !reponse).(0)) (Regles.nb_vote_max- !nbessais));
					reponse := joueurs.(id)#pose_question (idq,[|!tour|]);
					incr nbessais
					done;
				if !nbessais = Regles.nb_vote_max (*vote contre lui meme [regle n°3] *)
					then (vote.(id)<-vote.(id)+1;v_print_string 3 (Printf.sprintf "Arbitre: %i vote contre lui meme car il a dépassé la barre des %i votes incorrects\n" id Regles.nb_vote_max))
					else 
						begin
						vote.((snd !reponse).(0))<- vote.((snd !reponse).(0)) + 1 ;
						v_print_string 3 (Printf.sprintf "Arbitre: %i vote contre %i\n" id (snd !reponse).(0));
						communication_du_vote (condition_de_vote) c_nbjoueurs joueurs (4,[|id_vote;type_vote;!tour; id;(snd !reponse).(0) |]) 
						end
				end
			done;
		let (vict,maj) = vote_majorite vote in majorite:=maj ; victime:=vict;
		v_print_string 2 (Printf.sprintf "Arbitre: majorité: %b, tour: %i\n" !majorite !tour);
		incr tour
		done;
	((!victime, !nb_votants):int*int)
;;