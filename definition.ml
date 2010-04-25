(**Contient toutes les fonctions qui peuvent être appelées par tout le monde, joueurs y compris
On trouve notament les fonctions d'affichages, de tests standards, de personnalités et les fonctions de vote.
Le type joueur est défini ici, tous les joueurs doivent hériter de cette classe pour pouvoir jouer et être gérés par le conteur*)

(**Quantité d'éléments affichés par le programme
{ol list}
- 0 correspond à un affichage total et n'est recommandé que pour la recherche d'erreur.
- 1 correspond à l'affichage de tous les affichages proposés par l'arbitre.
- 2 correspond à une utilisation standard pour observer une partie et ses dessous.
- 3 correspond à un affichage selon un point de vue d'un joueur standard.
- 4 correspond à un affichage minimal pour vérifier qu'une partie se déroule bien (avec avertissements)
- 5 n'affiche rien sauf messages d'erreurs. (sans avertissements)

Cette liste est à jour dans la page du wiki UsingExecutable
*)
let verbose=ref 1;;



(**Fonctions d'affichage*)

(**Affichage d'une chaîne standard*)
let v_print_string level str=if !verbose<= level then print_string str;;
(**Affichage des chaînes acceptant la syntaxe des Printf.printf c'est à dire avec paramètres*)
let v_print level=if !verbose<= level then Printf.printf else Printf.ifprintf stdout;;

(**Affichage d'une chaîne dans un beau cadre *)
(*TODO ca semble hard à faire, a priori on laisse tomber et on met à chaque fois ce qu'il faut
la seule solution consiste à comprendre le type magique qui opère pour printf.printf*)

(**Affichage d'un tableau d'entier, puis passage à la ligne*)
let print_int_array level tab = 
if !verbose <= level
then (Array.iter (fun x->Printf.printf "%i " x) tab;
print_newline () );;

(**Affichage d'un tableau de booléens _unused_*)
let print_bool_array level tab=
    if !verbose <= level then
    (for i=0 to Array.length tab -1 do if tab.(i) then print_string "true " else print_string "false " done;
    print_string "\n";
    flush stdout)
    ;;

(**Autres fonctions inutiles*)

(**Teste s'il existe un élément du tableau vérifiant un prédicat*)
(*Attention le prédicat porte sur l'élément du tableau*)
let array_exists predicat tab=
    let i=ref 0 in
    while !i<Array.length tab && not (predicat tab.(!i)) do incr i done;
    not (!i=Array.length tab)
;;

(**Teste si tout les éléments d'un tableau vérifient un prédicat*)
(*Attention le prédicat porte sur l'élément du tableau*)
let array_all predicat tab= not (array_exists (fun x->not (predicat x)) tab);;

(**Teste s'il existe un élément du tableau vérifiant un prédicat*)
(*Attention le prédicat porte sur l'indice du tableau*)
let array_exists_2 predicat tab=
    let i=ref 0 in
    while !i<Array.length tab && not (predicat !i) do incr i done;
    not (!i=Array.length tab)
;;
(**Teste si tout les éléments d'un tableau vérifient un prédicat*)
(*Attention le prédicat porte sur l'indice du tableau*)
let array_all_2 predicat tab= not (array_exists_2 (fun x->not (predicat x)) tab);;


(**----------------------------------------------------------------*)
(** Début des définitions *)


(**Définition du type des personnalités des joueurs*)
type perso = Unknown |Loup | Villageois| Voyante| Sorciere|Mort of perso;;

(**Définition de la structure d'une information échangée de la forme id_information * information*)
type information=int*(int array);;

(**Conversion d'un type en la personnalité correspondante et réciproquement*)
(** *)
let int2perso n=
    match n with |0->Unknown |1->Loup|2->Villageois|3->Voyante |4->Sorciere|_->assert false
;;
let rec perso2int pers=
    match pers with 
        |Unknown -> 0 |Loup->1|Villageois->2 |Voyante->3|Sorciere->4
        |Mort sthing ->(v_print_string 4 "vous avez demandé l'identification perso2int d'un mort attention, (issue19 ?)\n";perso2int sthing)
;;

(**Conversion d'une personnalité en la chaîne associée*)
let rec perso2string pers=
    match pers with |Unknown -> "Unknown" | Loup ->"Loup"|Villageois->"Villageois"|Voyante->"Voyante"|Sorciere->"Sorciere"|Mort persbis-> (perso2string persbis)^" (Mort)"
;;

let rec string2perso str=
    match String.lowercase str with
    |"unknown" -> Unknown | "loup" -> Loup | "villageois" -> Villageois | "voyante" -> Voyante | "sorciere" -> Sorciere | _ -> failwith (Printf.sprintf "string2perso : %s n'est pas reconnu" str)
;;

(**Affichage d'un tableau de personnalités*)
let print_perso_array tab=
    Array.iter (fun x->( v_print 2 "%s " (perso2string x)) ) tab;print_newline ()
;;

(**Teste si une personnalité est morte ou non*)
let perso_is_dead  pers=match pers with Mort _->true |_->false;;

(**Classe des joueurs: dans chaque module, le joueur définit sa sous classe avec sa manière propre de répondre aux questions et d'assimiler les informations 
ici sont définies les éléments essentiels utilisés par le conteur
*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    (** Chaque joueur connaît le nombre de participants à la partie*)
    val nbjoueurs = c_nbjoueurs
    (** Un joueur est identitifié par son id*)
    val id = numjoueur
    (** Cette valeur contient le nom de la sous-classe du joueur*)
    val virtual classe : string
    (**Les deux fonctions les plus importantes d'un joueur sont définies :*)
    (**La méthode pour assimiler les informations données par le conteur*)
    method virtual donne_info : information -> unit
    (**La méthode pour répondre aux questions du conteur*)
    method virtual pose_question : information -> information
    (**enfin, des méthodes pour répondre aux questions très standards du conteur, indépendante de la classe du joueur, ce qui empêche certaines formes de triche*)
    (** donner sa sous-classe*)
    method get_classe = classe
    (**donner son identifiant*)
    method get_id = (id:int)
    (** donner le nombre de joueurs dans la partie (utile suelement pour le joueur lui-même)*)
    method get_nbjoueurs = (nbjoueurs:int)
  end;;
 
(**Définition des fonctions qui ne sont utiles qu'au conteur*)

(**Donne la répartition des LG dans lors de la distribution des rôles*)
let carte_LG nb_joueurs =
    (* je sais cette  manière d'écrire est contraire à l'idée CaMLique du match...with mais c'est plus lisible*)
    match nb_joueurs with
    |_ when nb_joueurs < 12 -> (>) 5 
    |_ when nb_joueurs < 17 -> (>) 6
    |_ when nb_joueurs < 22 -> (>) 7
    |_ -> (>) (2+(nb_joueurs-2)/5)

(**Renvoie un tableau pour un ordre aléatoire des joueurs afin de pouvoir leur donner une personnalité au hasard*)
let generer_ordre_aleatoire nb_joueurs =
    let ordre = Array.init nb_joueurs (fun i->i) in
    let permut i1 i2= (let tmp=ordre.(i1) in ordre.(i1) <- ordre.(i2); ordre.(i2) <- tmp) in
    for i=0 to nb_joueurs* 2 do 
         permut (Random.int nb_joueurs) (Random.int nb_joueurs) (*Permutations, on l'espère, aléatoires pour mélanger les joueurs*)
        done;
    ordre
;;

(** Attribue aléatoirement une personnalité à chacun des joueurs*)
let repartition nb_joueurs=
    if nb_joueurs < Regles.nb_joueurs_min
        then failwith (Printf.sprintf "Le nombre de joueurs est insuffisant pour attribuer correctement les rôles") (*Règle n°6*);
    let rep= generer_ordre_aleatoire nb_joueurs in
    (*on a désormais un tableau aléatoire avec les id des joueurs*)
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

(** Détermine si le vote se conclue par une majorité absolue et de toute façon le joueur plébiscité*)
let vote_majorite (resultats:int array)=
    let imaxl=ref [0] and sum=ref resultats.(0) in (*on stocke la liste des joueurs les plus plébiscités, qui comporte eventuellement des joueurs à égalité*)
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

(**Transmet aux votants les résultats du vote*)
let communication_du_vote (condition_de_vote: (int ->bool)) c_nbjoueurs joueurs info_a_transmettre=
for id=0 to c_nbjoueurs-1 do if condition_de_vote id then joueurs.(id)#donne_info info_a_transmettre done
;;

(**Fonction gèrant l'intégralité d'un vote*)
let appel_au_vote (condition_de_vote: (int ->bool)) (vote_invalide:information->bool) c_nbjoueurs joueurs idq id_vote type_vote=
    let urne = Array.make c_nbjoueurs 0 and tour=ref 1 and majorite_absolue = ref false and victime=ref (-1) and nb_votants = ref 0 in
    (*on informe que le vote commence*)
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|0|]) done; 
    while !tour <= 2 && (not !majorite_absolue) do
    (*majorite absolue au 1er tour ou relative au second    [ règle n°1] *)
        (*remise a zéro des votes après un eventuel premier tour*)
        for id=0 to c_nbjoueurs-1 do urne.(id)<- 0 done; 
        (* Vote des joueurs*)
        for id=0 to c_nbjoueurs-1 do
            if condition_de_vote id then
                begin
                incr nb_votants;
                (*Reccueil du vote du joueur*)
                let reponse=ref (joueurs.(id)#pose_question (idq,[|!tour|])) and nbessais=ref 1 in
                (*Vérification de la validité du vote*)
                while vote_invalide !reponse && !nbessais < Regles.nb_vote_max do (*correction issue6: vote contre un mort*)
                    ( v_print 2 "Arbitre: %i vote de façon invalide (contre %i), il n'a plus que %i essais avant de voter contre lui même\n" id ((snd !reponse).(0)) (Regles.nb_vote_max- !nbessais));
                    reponse := joueurs.(id)#pose_question (idq,[|!tour|]);
                    incr nbessais
                    done;
                    
                (* Application de la règle n°3 : vote contre soi-même*)
                if !nbessais = Regles.nb_vote_max
                    then (urne.(id)<-urne.(id)+1;( v_print 3 "Arbitre: %i vote contre lui-même car il a dépassé la barre des %i votes incorrects\n" id Regles.nb_vote_max))
                    else
                        (*Prise en compte du vote*)
                        begin
                        urne.((snd !reponse).(0))<- urne.((snd !reponse).(0)) + 1 ;
                        ( v_print 2 "Arbitre: %i vote contre %i\n" id (snd !reponse).(0));
                        (*Communication du vote à tous les joueurs concernés*)
                        communication_du_vote (condition_de_vote) c_nbjoueurs joueurs (4,[|id_vote;type_vote;!tour; id;(snd !reponse).(0) |]) 
                        end
                end
            done;

        (*Désignation de la victime*)
        let (vict,maj) = vote_majorite urne in 
        majorite_absolue :=maj ; 
        victime:=vict;
        ( v_print 3 "Conteur: %s, tour: %i\n" (if !majorite_absolue then "Majorité absolue" else "Pas de majorité absolue") !tour);
        (*on informe que le tour est fini*)
        for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|!tour|]) done;
        incr tour
        done;
    
    (*on informe que le vote est fini*)
    for id=0 to c_nbjoueurs -1 do if condition_de_vote id then joueurs.(id)#donne_info (6,[|3|]) done;

    (*Renvoi du résultat*)
    ((!victime, !nb_votants):int*int)
;;
