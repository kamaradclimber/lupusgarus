let verbose=3;;

let seed = int_of_float (Unix.time ());;

Random.init (seed);;


(*fonctions d'affichage*)
let v_print_string level str=if verbose<= level then print_string str;;
let v_print level=if verbose<= level then Printf.printf else Printf.ifprintf stdout;;

v_print 4 "Arbitre: L'initialisation al�atoire est %i" seed;;
(*Ainsi si un probl�me apparait, on peut r�cr�er exactement les m�mes conditions pour v�rifier si on l'a bien corrig�, il suffit d'imposer la seed � la valeur probl�matique*)

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
        |Mort sthing ->(v_print_string 4 "vous avez demand� l'identification perso2int d'un mort attention, (issue19 ?)\n";perso2int sthing)
;;
let rec perso2string pers=
    match pers with |Unknown -> "Unknown" | Loup ->"Loup"|Villageois->"Villageois"|Voyante->"Voyante"|Sorciere->"Sorciere"|Mort persbis-> (perso2string persbis)^" (Mort)"
;;
let print_perso_tab tab=
    Array.iter (fun x->( v_print 2 "%s " (perso2string x)) ) tab;print_newline ()
;;

let perso_is_dead  pers=match pers with Mort _->true |_->false;;

(*classe des joueurs: dans chaque module, le joueur d�finit sa sous classe avec sa mani�re propre de r�pondre aux questions et d'assimiler les informations*)

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
 
let repartition nbjoueurs=
(* Attribue al�atoirement une personnalit� � chacun des joueurs*)
    if nbjoueurs < Regles.nb_joueurs_min
        then failwith (Printf.sprintf "Le nombre de joueurs est insuffisant pour attribuer correctement les r�les") (*R�gle n�6*);
    let rep=Array.init nbjoueurs (fun i->i) in
    let perm tab i1 i2= let tmp=tab.(i1) in tab.(i1)<-tab.(i2); tab.(i2)<-tmp in
    for i=0 to 2*nbjoueurs do perm rep (Random.int nbjoueurs) (Random.int nbjoueurs) done; (*Permutations, on l'esp�re, al�atoires pour m�langer les joueurs*)
    (*on a desormais un tableau al�atoire avec les id des joueurs*)
    let rep2=Array.make nbjoueurs Villageois in
    for i=0 to nbjoueurs-1 do 
        match i mod 3 with
            |0->rep2.(i)<- Loup (*Un joueur sur 3 est un Loup*)
            |_->()
        done;
    rep2.(1)<- Voyante;
    rep2.(2)<- Sorciere;
    let rep3=Array.make nbjoueurs Unknown in
    for i=0 to nbjoueurs-1 do rep3.(rep.(i))<-rep2.(i) done;
    rep3
;;

let vote_majorite (resultats:int array)=
(* d�termine si le vote se conclue par une majorit� absolue et de toute fa�on le joueur pl�biscit�*)
    let imaxl=ref [0] and sum=ref 0 in (*on stocke la liste des joueurs les plus pl�biscit�s, qui comporte eventuellement des joueurs � �galit�*)
    for i=1 to Array.length resultats-1 do
        sum:=!sum+ resultats.(i); (*on stocke le nombre de votes au total*)
        match compare resultats.(List.hd !imaxl) resultats.(i) with
            |0-> imaxl := (i::(!imaxl))
            |(-1)->imaxl:=[i]
            |_-> ()
        done;
    let tmp=Array.of_list !imaxl in let n=Array.length tmp in 
    let choix=tmp.(Random.int n) (*si egalite, le hasard decide [r�gle n�2 ] +  correction issue8 *) in
    (choix, resultats.(choix) > (!sum)/2) (*joueur pl�biscit�, majorit� absolue*)
;;

let communication_du_vote (condition_de_vote: (int ->bool)) c_nbjoueurs joueurs info_a_transmettre=
(*transmet aux votants les r�sultats du vote*)
for id=0 to c_nbjoueurs-1 do if condition_de_vote id then joueurs.(id)#donne_info info_a_transmettre done
;;

let appel_au_vote (condition_de_vote: (int ->bool)) (vote_invalide:information->bool) c_nbjoueurs joueurs idq id_vote type_vote=
(*g�re l'int�gralit� d'un vote*)
    let urne = Array.make c_nbjoueurs 0 and tour=ref 1 and majorite_absolue = ref false and victime=ref (-1) and nb_votants = ref 0 in
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|0|]) done; (*on informe que le vote commence*)

    while !tour <= 2 && (not !majorite_absolue) do
    (*majorite absolue au 1er tour ou relative au second    [ r�gle n�1] *)
        for id=0 to c_nbjoueurs-1 do urne.(id)<- 0 done; (*remise a z�ro des votes apr�s un eventuel premier tour*)

        for id=0 to c_nbjoueurs-1 do
            if condition_de_vote id then
                begin
                incr nb_votants;
                let reponse=ref (joueurs.(id)#pose_question (idq,[|!tour|])) and nbessais=ref 1 in
                while vote_invalide !reponse && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
                    ( v_print 2 "Arbitre: %i vote de fa�on invalide (contre %i), il n'a plus que %i essais avant de voter contre lui m�me\n" id ((snd !reponse).(0)) (Regles.nb_vote_max- !nbessais));
                    reponse := joueurs.(id)#pose_question (idq,[|!tour|]);
                    incr nbessais
                    done;
                if !nbessais = Regles.nb_vote_max (*vote contre lui meme [r�gle n�3] *)
                    then (urne.(id)<-urne.(id)+1;( v_print 3 "Arbitre: %i vote contre lui-m�me car il a d�pass� la barre des %i votes incorrects\n" id Regles.nb_vote_max))
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
        ( v_print 3 "Arbitre: majorit�: %b, tour: %i\n" !majorite_absolue !tour);
        for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|!tour|]) done; (*on informe que le tour est fini*)
        incr tour
        done;
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|3|]) done; (*on informe que le vote est fini*)
    ((!victime, !nb_votants):int*int)
;;
