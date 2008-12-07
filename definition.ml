let verbose=3;;

let seed = int_of_float (Unix.time ());;

Random.init (seed);;


(*fonctions d'affichage*)
let v_print_string level str=if verbose<= level then print_string str;;
let v_print level=if verbose<= level then Printf.printf else Printf.ifprintf stdout;;

v_print 4 "Arbitre: L'initialisation aléatoire est %i\n" seed;;
(*Ainsi si un problème apparait, on peut récréer exactement les mêmes conditions pour vérifier si on l'a bien corrigé, il suffit d'imposer la seed à la valeur problématique*)

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
        |Mort sthing ->(v_print_string 4 "vous avez demandé l'identification perso2int d'un mort attention, (issue19 ?)\n";perso2int sthing)
;;
let rec perso2string pers=
    match pers with |Unknown -> "Unknown" | Loup ->"Loup"|Villageois->"Villageois"|Voyante->"Voyante"|Sorciere->"Sorciere"|Mort persbis-> (perso2string persbis)^" (Mort)"
;;
let print_perso_tab tab=
    Array.iter (fun x->( v_print 2 "%s " (perso2string x)) ) tab;print_newline ()
;;

let perso_is_dead  pers=match pers with Mort _->true |_->false;;

(*classe des joueurs: dans chaque module, le joueur définit sa sous classe avec sa manière propre de répondre aux questions et d'assimiler les informations*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    val nbjoueurs = c_nbjoueurs
    val id = numjoueur
    val virtual classe : string
    method virtual donne_info : information -> unit
    method virtual pose_question : information -> information
    method get_classe = classe
    method get_id = (id:int)
    method get_nbjoueurs = (nbjoueurs:int)
  end;;
 
let carte_LG nb_joueurs =
    (* je sais cette  manière d'écrire est contraire à l'idée CaMLique du match...with mais cets plus lisible*)
    match nb_joueurs with
    |_ when nb_joueurs < 12 -> (>) 5 
    |_ when nb_joueurs < 17 -> (>) 6
    |_ when nb_joueurs < 22 -> (>) 7
    |_ -> (>) (2+(nb_joueurs-2)/5)

let generer_ordre_aleatoire nb_joueurs =
    let ordre = Array.init nb_joueurs (fun i->i) in
    let permut i1 i2= (let tmp=ordre.(i1) in ordre.(i1) <- ordre.(i2); ordre.(i2) <- tmp) in
    for i=0 to nb_joueurs* 2 do 
         permut (Random.int nb_joueurs) (Random.int nb_joueurs) (*Permutations, on l'espère, aléatoires pour mélanger les joueurs*)
        done;
    ordre
;;


let repartition nb_joueurs=
(* Attribue aléatoirement une personnalité à chacun des joueurs*)
    if nb_joueurs < Regles.nb_joueurs_min
        then failwith (Printf.sprintf "Le nombre de joueurs est insuffisant pour attribuer correctement les rôles") (*Règle n°6*);
    let rep= generer_ordre_aleatoire nb_joueurs in
    (*on a desormais un tableau aléatoire avec les id des joueurs*)
    let rep2=Array.make nb_joueurs Villageois in
    for i=0 to nb_joueurs-1 do 
        if carte_LG nb_joueurs i then rep2.(i)<- Loup else () (*si le joueur pioche les premières cartes du paquet, il est LG !*)
        done;
    rep2.(1)<- Voyante;
    rep2.(2)<- Sorciere;
    let rep3=Array.make nb_joueurs Unknown in
    for i=0 to nb_joueurs-1 do rep3.(rep.(i))<-rep2.(i) done;
    rep3
;;

let vote_majorite (resultats:int array)=
(* détermine si le vote se conclue par une majorité absolue et de toute façon le joueur plébiscité*)
    let imaxl=ref [0] and sum=ref 0 in (*on stocke la liste des joueurs les plus plébiscités, qui comporte eventuellement des joueurs à égalité*)
    for i=1 to Array.length resultats-1 do
        sum:=!sum+ resultats.(i); (*on stocke le nombre de votes au total*)
        match compare resultats.(List.hd !imaxl) resultats.(i) with
            |0-> imaxl := (i::(!imaxl))
            |(-1)->imaxl:=[i]
            |_-> ()
        done;
    let tmp=Array.of_list !imaxl in let n=Array.length tmp in 
    let choix=tmp.(Random.int n) (*si egalite, le hasard decide [règle n°2 ] +  correction issue8 *) in
    (choix, resultats.(choix) > (!sum)/2) (*joueur plébiscité, majorité absolue*)
;;

let communication_du_vote (condition_de_vote: (int ->bool)) c_nbjoueurs joueurs info_a_transmettre=
(*transmet aux votants les résultats du vote*)
for id=0 to c_nbjoueurs-1 do if condition_de_vote id then joueurs.(id)#donne_info info_a_transmettre done
;;

let appel_au_vote (condition_de_vote: (int ->bool)) (vote_invalide:information->bool) c_nbjoueurs joueurs idq id_vote type_vote=
(*gère l'intégralité d'un vote*)
    let urne = Array.make c_nbjoueurs 0 and tour=ref 1 and majorite_absolue = ref false and victime=ref (-1) and nb_votants = ref 0 in
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|0|]) done; (*on informe que le vote commence*)

    while !tour <= 2 && (not !majorite_absolue) do
    (*majorite absolue au 1er tour ou relative au second    [ règle n°1] *)
        for id=0 to c_nbjoueurs-1 do urne.(id)<- 0 done; (*remise a zéro des votes après un eventuel premier tour*)

        for id=0 to c_nbjoueurs-1 do
            if condition_de_vote id then
                begin
                incr nb_votants;
                let reponse=ref (joueurs.(id)#pose_question (idq,[|!tour|])) and nbessais=ref 1 in
                while vote_invalide !reponse && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
                    ( v_print 2 "Arbitre: %i vote de façon invalide (contre %i), il n'a plus que %i essais avant de voter contre lui même\n" id ((snd !reponse).(0)) (Regles.nb_vote_max- !nbessais));
                    reponse := joueurs.(id)#pose_question (idq,[|!tour|]);
                    incr nbessais
                    done;
                if !nbessais = Regles.nb_vote_max (*vote contre lui meme [règle n°3] *)
                    then (urne.(id)<-urne.(id)+1;( v_print 3 "Arbitre: %i vote contre lui-même car il a dépassé la barre des %i votes incorrects\n" id Regles.nb_vote_max))
                    else 
                        begin (*prise en compte du vote*)
                        urne.((snd !reponse).(0))<- urne.((snd !reponse).(0)) + 1 ;
                        ( v_print 2 "Arbitre: %i vote contre %i\n" id (snd !reponse).(0));
                        communication_du_vote (condition_de_vote) c_nbjoueurs joueurs (4,[|id_vote;type_vote;!tour; id;(snd !reponse).(0) |]) 
                        end
                end
            done;

        let (vict,maj) = vote_majorite urne in 
        majorite_absolue :=maj ; 
        victime:=vict;
        ( v_print 3 "Arbitre: majorité: %b, tour: %i\n" !majorite_absolue !tour);
        for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|!tour|]) done; (*on informe que le tour est fini*)
        incr tour
        done;
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|3|]) done; (*on informe que le vote est fini*)
    ((!victime, !nb_votants):int*int)
;;
