(** Ce fichier est le coeur du programme car il repr\130sente le maître du jeu qui distribue les rôles, g\138re les parties et narre l'histoire.
Il peut \130galement servir d'arbitre.
Lui seul connaît les identit\130s des joueurs et peut les appeler pour les informer ou leur poser des questions. Pour celà, il est compil\130 en dernier afin de ne pas pouvoir être appel\130 par les autres joueurs (il est le seul à avoir un pointeur vers tous les joueurs
*)
open Definition


(**D\130finition du nombre de joueurs*)
let c_nbjoueurs = 10
;;  
(**Ce nombre de joueurs ne doit pas être inf\130rieur à une borne d\130finie dans le fichier regles.ml*)
if c_nbjoueurs<Regles.nb_joueurs_min then 
    v_print_string 4 "Arbitre: des erreurs peuvent survenir, le nombre de joueurs est trop faible\n"
;;
(**Tableau contenant un la personnalit\130 de chaque joueur*)
let c_whoswho = Array.make c_nbjoueurs Unknown (*Ce tableau est index\130 par l'id des joueurs*)
;;
(** Contient les potions disponibles pour la sorci\138re
[| potion de vie; poison|]*)
(*Ceci ne sera valide tant qu'il n'y qu'une sorci\138re*)
let c_potions = [|1;1|] 
;;
(**Contient l'identifiant du type de fin de partie
-1 correspond à une valeur par d\130faut tant que ce n'est pas la fin de la partie*)
let id_end = ref (-1)
;;
(**Indique le num\130ro du vote afin de pouvoir s'y rep\130rer*)
let id_vote = ref 0
;;
(**Permet de savoir si un joueur est de telle personnalit\130*)
let c_is perso id = Definition.is perso c_whoswho.(id);;

let array_filter predicat tab=
    let resultat= ref [] in (* <- bouh comme c'est pas beau d'utiliser une pile sans le dire !*)
    for i=0 to Array.length tab -1 do
        if predicat tab.(i) then resultat := i :: !resultat
        done;
    !resultat

(**Fonction trouvant l'autre amoureux dans le whoswho*)
let find_other_amoureux am1=    
    let amoureux = array_filter (Definition.is CAmoureux) c_whoswho in
    match amoureux with
        | [] -> failwith "pas d'amoureux ??"
        |[a1;a2] when a1=am1 -> a2
        |[a1;a2] when a2=am1 -> a1
        | _ -> assert false
;;

(**Fonction trouvant le chasseur dans le whoswho*)
let find_chasseur ()=
    let chasseurs = array_filter (Definition.is (C Chasseur)) c_whoswho in
    match chasseurs with
    | [] -> failwith "pas de chasseur !"
    | [a] -> a
    | _-> assert false
;;
(**Fonctions assurant les conversions des joueurs en objets de classe Definition.joueur,
cette conversion permet la coercion, cest à dire d'indiquer au v\130rificateur de type d'ocaml que telle sous-classe de joueur sera consideree exclusivement comme un joueur tout court*)
(** *)
let reliable2j reliable = (reliable: Joueur2.reliable :> Definition.joueur)
let conf2j confiant = (confiant: Joueur3.confiant :> Definition.joueur) 
let jdb j = (j: Joueur.joueur_de_base :> Definition.joueur)
(*let prob2j conf = (conf: Joueur4.probabiliste :> Definition.joueur)*)
;;
(** Tableau contenant un pointeur vers chaque joueur,
il est ici initialis\130 avec des joueurs de type confiant*)
let joueurs = Array.init c_nbjoueurs ( fun i-> conf2j (new Joueur3.confiant c_nbjoueurs i))
;;
(** Pile contenant les morts qui ne sont pas encore enregistr\130s comme morts mais qui ont \130t\130s tu\130s.
Il s'agit par exemple de la victime des Loup-Garous ou de la sorci\138re*)
let morgue=((Stack.create ()): (int*int) Stack.t)
;;
(** Tableau contenant un ordre al\130atoire pour affecter une personnalit\130 au hasard à chacun des joueurs
Ce tableau est utilis\130 lorsque l'on veut parcourir les joueurs afin de ne pas permettre l'identification de la personnalit\130 d'un joueur en regardant le moment où telle personnalit\130 est appel\130e cf issue14 *)
let ordre = generer_ordre_aleatoire c_nbjoueurs;; 

(**-----------------------------------------------------------------------------------------------*)
(** D\130but des fonctions d\130finissant les phases de la partie*)

(**Initialisation du jeu: distribue les rôles, demande a chaque joueur de s'initialiser en cons\130quence*) 
let initialisation () =
    
    (**R\130partition des joueurs*)
    let reparti = repartition c_nbjoueurs in 
    v_print_string 2 "Conteur: R\130partition des joueurs:\n";
    print_perso_array reparti;
    let idi8_tab = idi8 reparti in
    print_int_array 2 idi8_tab;
    (*Information des joueurs de leur affectation et du nombre de joueurs interpr\130tant chaque personnalit\130*)
    for id=0 to c_nbjoueurs-1 do 
        joueurs.(id)#donne_info (8,idi8_tab);
        joueurs.(id)#donne_info (1,[|id;perso2int reparti.(id)|]);
        c_whoswho.(id) <- reparti.(id);
        
        (*et v\130rification qu'ils ont bien compris
        cependant cette v\130rification n'est pas suivie d'une exception en cas d'erreur*)
        let (_,reponse)=joueurs.(id)#pose_question (1,[|id|]) in
        ( v_print 1 "Arbitre: joueur %i  s'identifie comme %i etant un %s ce qui est %b\n" id reponse.(0) (perso2string ( int2perso reponse.(1))) (int2perso reponse.(1)= reparti.(id) && id=reponse.(0)))
        done;
    if !Definition.verbose <= 4 
    then
        let chaine= "Conteur: le jeu commence" in
        let n = String.length chaine in
        let cadre = String.make (n+2) '-' in
        print_string (cadre^"\n"^chaine^" |\n"^cadre^"\n")
        ;
    (*Cupidon ca d\130signer les amoureux*)
    v_print_string 3 "Conteur: Cupidon se r\135veille et d\130coche une fl\136che aux deux amoureux\n";
        let cupidon = ref (-1) in
        for id=0 to c_nbjoueurs-1 do 
            if c_is (C Cupidon) id then cupidon := id
            done;
        if !cupidon <> -1 (*au cas où il n'y a pas de cupidon*)
            then 
                let  (_, noms) = joueurs.(!cupidon)#pose_question (7,[||]) in
                let am1 = noms.(0) and am2 = noms.(1) in
                if am1 = am2
                then v_print 2 "Arbitre: %i (Cupidon) est pas malin, il d\130signe %i comme \130tant amoureux de lui-même, sa flêche ne sera pas prise en compte\n" !cupidon am1
                else begin
                    v_print_string 3 "Je vais pr\130venir les deux amoureux, ils vont ouvrir les yeux et se reconnaitrent\n";
                    v_print 4 "Conteur: Les amoureux sont %i (%s) et %i (%s)\n" am1 (perso2string c_whoswho.(am1)) am2 (perso2string c_whoswho.(am2));
                    (*On lui dit qu'il est amoureux de untel, il doit alors se mettre à jour, mais ne sais pas qu'elle est l'identit\130 de l'autre*) 
                    joueurs.(am1)#donne_info (7, [|am1;am2|]);
                    joueurs.(am2)#donne_info (7, [|am1;am2|]);
                    
                    (*Le conteur se met à jour*)
                    c_whoswho.(am1) <- Amoureux c_whoswho.(am1);
                    c_whoswho.(am2) <- Amoureux c_whoswho.(am2);
                    v_print_string 3 "Conteur: Les amoureux peuvent d\130sormais se rendormir\n"
                    end;
    (*Les LG se reconnaissent entre eux, c'est à dire qu'on les informe de l'identit\130 des autres LG*)
    v_print_string 3 "Conteur: Les loups-garous vont se reconnaîtrent, ils ouvrent les yeux\n";
    for id=0 to c_nbjoueurs-1 do
        for id2=id+1 to c_nbjoueurs-1 do
            if c_is (C Loup) id && c_is (C Loup) id2 then (joueurs.(id)#donne_info (1,[|id2;perso2int Loup|]); joueurs.(id2)#donne_info (1,[|id;perso2int Loup|]))
            done
        done;
    v_print_string 3 "Conteur: Les loups-garous se sont reconnus, ils se rendorment en se l\130chant les babines \133 la pens\130e des festins futurs\n"
;;


(**Fonction testant si la partie doit se terminer*)
let is_it_the_end () =
(*l'indentation de cette fonction n'est pas canonique mais est plus lisible qu'un escalier*)

    (*tout le monde est mort*)
    if array_all (Definition.is CMort) c_whoswho                    
        then (id_end := 0; true)
        
    (*tout les loups sont morts*)
    else if array_all (fun pers->not (Definition.is (C Loup) pers)) c_whoswho  
        then (id_end := 1; true)
        
    (*les loups garous gagnent la partie*)
    else if array_all (fun pers->Definition.is (C Loup) pers || Definition.is CMort pers) c_whoswho 
        then (id_end := 2; true)
        
    (*Les amoureux gagnent la partie*)
    else if array_all (fun pers -> Definition.is CMort pers || Definition.is CAmoureux pers) c_whoswho 
        then (id_end := 3; true)
        
    (*la partie n'est pas finie*)
    else false                                                                
;;


(** Fonction g\138rant la fin du jeu: affiche les gagnants, le rôle de chacun...*)
let epilogue () =
    (match !id_end with
        |0 -> v_print_string 3 "\nConteur: Tout le monde est mort, le village de Salem s'est entretu\130 !\n"
        |1 -> v_print_string 3 "\nConteur: Tous les loups garou sont morts, le village de Salem est sauv\130 !\n"
        |2 -> v_print_string 3 "\nConteur: Tous les villageois sont morts, le village de Salem est tomb\130 aux mains du mal !\n"
        |3 -> v_print_string 3 "\nConteur: Seuls les amoureux sont encore vivants, ils vivent heureux, ensemble, pour une longue \130ternit\130...\n"
        |_ -> v_print_string 4 "\nArbitre: Le jeu a quitt\130 pour une raison inconnue"
    );
    
    (*Affichage des rôles des participants*)
    if !Definition.verbose <= 3 
    then
        let chaine= "Conteur: La partie est termin\130e\n les r\147les distribu\130s \130taient les suivants" in
        let n = String.length chaine in
        let cadre = String.make (n+2) '-' in
        print_string (cadre^"\n"^chaine^" |\n"^cadre^"\n")
        ;
    
    (*Affichage \130quipe par \130quipe*)
    Array.iteri (fun i-> fun perso ->( if perso = Mort Loup || perso = Loup then v_print 3 "%i \130tait %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    v_print_string 3 "------------------\n";
    Array.iteri (fun i-> fun perso ->( if  not (perso = Mort Loup || perso = Loup || Definition.is CAmoureux perso) then v_print 3 "%i \130tait %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    v_print_string 3 "------------------\n";
    Array.iteri (fun i-> fun perso ->( if  Definition.is CAmoureux perso then v_print 3 "%i \130tait %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    
;;


let rec autopsie morgue cadavre =
(* d\130finition de la nouvelle morgue: 
il s'agit de stocker en plus la raison du d\130c\138s et si besoin est, comment on en est arriv\130 là (par exemple un chasseur amoureux qui meurt de chagrin)
afin de pouvoir adapter les dialogues au mieux
on aura donc un entier qui expliquera tout (cf raisons de la mort dans syntaxeechange[...].wiki *)

(*Ce programme doit sugg\130rer plein de question dans les cas anormaux comme: la mort d'un amoureux tu\130 par le chasseur qui est mort de chagrin...
en effet, pour le moment on ne v\130rifie pas certaines r\130ponses comme le fait que le chasseur ne tue pas son amoureux....,
pour le moment les r\138gles doivent etre suffisament strictes pour que le programmme ne bugge pas mais les affichages ne sont pas forcement optimaux*)

(*Peut etre faudra-t-il indiquer en arguement le moment où l'autopsie est appel\130e entre jour / petit matin pour mieux adapter l'affichage <- en fait pas besoin car les cas de d\130c\138s sont suffisament explicites*)
    let (cause_du_deces, id_mort) =  cadavre in
    assert (not (c_is CMort id_mort ));
    
    (*Affichage de la cause de la mort du joueur*)
    begin
    match cause_du_deces with
        |1-> v_print 3 "Conteur: %i (%s) est mort pendant la nuit, son cadavre est d\130couvert au matin sur le pas de sa porte\n" id_mort (perso2string c_whoswho.(id_mort))
        |2-> if c_is (C Loup) id_mort 
        then v_print 3 "Conteur: %i (%s) est donc pendu en place publique, sa mort est lente et douloureuse et tous les membres du village, en dansant autour\n        de la potence, esp\138rent avoir \130radiqu\130 le mal qui r\147de dans le village\n" id_mort (perso2string c_whoswho.(id_mort))
        else v_print 3 "Conteur: %i (%s) est donc pendu en place publique, sa mort est lente et douloureuse mais le village est d\130sesp\130r\130 d'avoir condamn\130 un innocent\n          alors que le mal r\147de toujours dans le village.\n" id_mort (perso2string c_whoswho.(id_mort))
        |3-> v_print 3 "Conteur: Dans son ultime agonie, le chasseur (%i) a pris son fusil et fait un head-shot sur %i (%s)\n" (find_chasseur ()) (*TODO*) id_mort (perso2string c_whoswho.(id_mort))
        |4-> v_print 3 "Conteur: Lorsque les villageois sont venus chercher le chasseur (%i), celui-ci a d\130gain\130 son fusil et abattu %i (%s)\n" (find_chasseur ()) id_mort (perso2string c_whoswho.(id_mort))
        |5-> v_print 3 "Conteur: Lorsque %i (%s) a d\130couvert le cadavre de son amoureux %i, une grande tristesse s'est empar\130e de lui et il va mettre fin \133 ses jours seul dans la forêt\n" id_mort (perso2string c_whoswho.(id_mort)) (find_other_amoureux id_mort)
        |6-> v_print 3 "Conteur: Apr\138s que %i aie \130t\130 pendu par l'ensemble du village, %i (%s) s'est retir\130, seul, dans la forêt et ne pouvant vivre sans %i a mis fin \133 ses jours\n" (find_other_amoureux id_mort) id_mort (perso2string c_whoswho.(id_mort)) (find_other_amoureux id_mort)
        |7-> v_print 3 "Conteur: %i ayant \130t\130 abattu par le chasseur, %i (%s) se fane de chagrin et se retire dans du monde discretement dans la for\136t\n" (find_other_amoureux id_mort) id_mort (perso2string c_whoswho.(id_mort))
        |8-> v_print 3 "Conteur: Avant d'aller se suicider, %i a sorti son fusil et exprim\130 son d\130sespoir en tirant sur %i (%s)\n" (find_chasseur ())id_mort (perso2string c_whoswho.(id_mort))
        |_ -> failwith (Printf.sprintf "%i est mort pour une raison inconnue (soulev\130 par autopsie dans conteur.ml)" id_mort)
    end;
    (*le conteur informe les joueurs des morts*)
    for id=0 to c_nbjoueurs-1 do 
    if not (c_is (CMort) id) then
        begin
        joueurs.(id)#donne_info (1,[|id_mort;perso2int c_whoswho.(id_mort)|]);
        joueurs.(id)#donne_info (3,[|id_mort;cause_du_deces|]);
        v_print_string 0 "information dun joueur\n"
        end
    done;
    (*maj des infos du conteur*)
    c_whoswho.(id_mort)<- Mort c_whoswho.(id_mort);

    (*Maintenant on g\138re les cas particuliers de mort*)
    if c_is CAmoureux id_mort && (match cause_du_deces with 5|6|7->false|_->true)
        then begin
            (**Recherche de l'autre amoureux*)
            let ame_soeur = find_other_amoureux id_mort in
            (*On a d\130sormais l'amoureux, on va le tuer en ajoutant la cause de mort en fonction de celle de l'autre amoureux*)
            (*On regarde d'abord si l'amoureux est d\130jà dans la morgue*)
            stack_filter (fun (c,id)-> id <> ame_soeur) morgue;
            match cause_du_deces with
                |1-> Stack.push (5,ame_soeur) morgue
                |2-> Stack.push (6,ame_soeur) morgue
                |3|4-> Stack.push (7,ame_soeur) morgue
                |5|6|7|8-> failwith "Si on en arrive l\133 c'est que l'autre amoureux est mort car son amoureux est mort"
                |_-> failwith "type de mort inconnu ! (cf autopsie dans conteur.ml)"
            end;
            (*on met des point-virgule entre les tests si jamais il y a plusieurs cas qui se pr\130sentent par exemple chasseur amoureux*)
    if c_is (C Chasseur) id_mort
        then begin
            let (_,reponse)= joueurs.(id_mort)#pose_question (2,[||]) in
            let id_gibier = reponse.(0) in
            stack_filter (fun (c,id)-> id <> id_gibier) morgue;
            match cause_du_deces with
                |1-> Stack.push (3,id_gibier) morgue
                |2-> Stack.push (4,id_gibier) morgue
                |5|6-> Stack.push (8,id_gibier) morgue
                |3|4-> failwith "Si on en arrive l\133 c'est qu'il y a deux chasseurs ou un gros bug"
                |7|8 -> failwith "Si on en arrive l\133 cest que le chasseur a tu\130 son amoureux ou alors il y a plusieurs chasseurs ou un gros bug"
                |_-> failwith "type de mort inconnu ! (cf autopsie dans conteur.ml)"
            end
;;

(**Fonction g\130rant la nuit: ordre des perso à faire jouer, actions de chacun*)
let nuit () =
    v_print_string 3 "Conteur:La nuit tombe\n";
    
    (*R\130veil des LG*)
    v_print_string 3 "Conteur:les loups-garous se r\130veillent et rodent pendant la nuit\n";
    let (victime, nb_votants)=
        appel_au_vote 
            (fun id -> not (c_is (CMort) id) && (c_is (C Loup) id))            (* D\130finition des votants*)
            (fun (_,contenu)->c_is (CMort) (contenu).(0) )               (* D\130finition d'un vote invalide*)
            c_nbjoueurs                                               (* Nombre de joueurs dans la partie*)
            joueurs                                                   (* Tableau des joueurs*)
            3                                                         (* Type de la question : idq *)
            !id_vote
            1                                                         (* Type de vote : de jour, de nuit...*)
        in
    
    (* Il y a eu un vote de plus: on incr\130mente le nombre de vote*)
    incr id_vote;
    
    (* Test pour prendre ou non le vote en compte*)
    if nb_votants>0 (*issue 10*) 
        then Stack.push (1,victime) morgue 
        else v_print_string 4 "Arbitre: il n'y a eu aucun votants pour le vote des loups-garous, la partie devrait être termin\130\130\n";


    v_print_string 3 "Conteur:Les loups-garous se rendorment\n";

    (*R\130veil des joueurs qui jouent la nuit*)
    let id = ref 0 in
    for i=0 to c_nbjoueurs - 1 do
        id := ordre.(i);
        
        (* C'est au tour de la sorci\138re de jouer*)
        if c_is (C Sorciere) !id
            then 
                begin
                v_print_string 3 "Conteur: la Sorci\138re se r\130veille\n";
                let (_,reponse) = joueurs.(!id)#pose_question (5,[|victime|]) in
                
                (*V\130rification de la validit\130 de la r\130ponse de la sorci\138re et prise en compte de sa d\130cision*)
                if reponse.(2)=1 && c_potions.(0) > 0
                    then 
                        begin
                        c_potions.(0)<- c_potions.(0) - 1; 
                        ignore(Stack.pop morgue);
                        v_print_string 2 "Arbitre: Sorci\138re utilise potions vie\n"
                        end;
                if reponse.(0)=1 && c_potions.(1) > 0
                    then 
                        begin
                        c_potions.(1)<- c_potions.(1) - 1; (*on enl\138ve de toute facon meme si la cible est morte [r\138gle n°7] *)
                        if not (c_is (CMort) reponse.(1))
                            then (Stack.push (1,reponse.(1)) morgue; v_print 2 "Arbitre: Sorci\138re utilise poison contre %i\n" reponse.(1))
                            else v_print 2 "Arbitre: Sorci\138re (%i) utilise poison contre %i qui est mort, une potion lui est retir\130e [r\138gle n°7]\n" !id reponse.(1);
                        end;
                        
                v_print_string 3 "Conteur: La sorci\138re se rendort\n"
                end;

        (*C'est au tour de la voyante de jouer*)
        if c_is (C Voyante) !id
            then (*on pourrait utiliser une procedure de vote un peu speciale pour \130conomiser des lignes de code mais ca serait moins clair*)
                begin
                v_print_string 3 "Conteur: la Voyante se reveille.. ....et me designe la personne dont elle veut sonder l'identit\130\n";
                let (_,reponse)= joueurs.(!id)#pose_question (4,[||]) in
                match c_whoswho.(reponse.(0)) with
                    | Unknown -> assert false
                    | Mort _ -> v_print_string 1 "Arbitre: la Voyante demande l'identit\130 d'un mort, issue13\n"
                    | pers -> 
                        begin
                        v_print 2 "Arbitre: La Voyante demande l'identification de %i qui est %s\n" reponse.(0) (perso2string c_whoswho.(reponse.(0))) ;
                        joueurs.(!id)#donne_info (1,[|reponse.(0);perso2int c_whoswho.(reponse.(0))|])
                        end;
                v_print_string 3 "Conteur: la voyante se rendort\n"
                end
    done

;;


(** Fonction g\130rant le lever du soleil (et oui c'est compliqu\130 !) c'est à dire annoncer les morts et officialiser les d\130c\138s*)
let petit_matin ()=
    v_print_string 3 "Conteur: Le jour se l\138ve...\n";
    while not (Stack.is_empty morgue) do
        let cadavre = Stack.pop morgue in
        autopsie morgue cadavre
        done
;;


(**Fonction g\130rant le jour: morts des personnages, action specifique, pendaison publique.../*)
let jour () =
    v_print_string 3 "Conteur: proc\130dons au vote\n";
    let (suspect,nb_votants)=appel_au_vote (fun id -> not (c_is (CMort) id) ) (fun (idq,contenu)->c_is (CMort) (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 2 !id_vote 0 in
    incr id_vote;
    if nb_votants>0 (*issue 10*)
        then begin
            Stack.push (2,suspect) morgue;
            while not (Stack.is_empty morgue) do
                let cadavre=Stack.pop morgue in
                autopsie morgue cadavre
                done
            end
        else v_print_string 4 "Arbitre: personne n'est mort, car il n'ya eu aucun votant, il doit yavoir un probl\138me (cf issue10)\n"
;;

(** On parse la ligne de commande pour éventuellement spécifier quelques paramètres*)
let speclist= [("-v",Arg.Int (fun verbose->Printf.printf "Mutisme du conteur : %i \n" verbose; Definition.verbose := verbose),"définit le niveau de mutisme du conteur et de l'arbitre, un entier est attendu, (0 volubile, 3 normal, 6 muet)");("-s",Arg.Int (fun seed->Random.init (seed);v_print 5 "Arbitre: La nouvelle initialisation aléatoire est %i\n" seed),"met remet l'aléatoire à une graine donnée")] in
Arg.parse speclist (fun (_:string)->()) "Description des quelques options proposées par le programme:";


(**D\130but du jeu*)
initialisation ();

(**D\130roulement de la partie*)
while not (is_it_the_end ()) do 
    nuit ();
    petit_matin ();
    if not (is_it_the_end ()) 
        then jour ();
    flush stdout 
    done
;;

(**Fin de la partie*)
epilogue ();;
v_print_string 4 "Conteur: le jeu se termine\n";;

(**Fin du programme*)
flush stdout;;
if Sys.os_type = "Unix"
then print_float (Sys.time ())
else ()
;;
print_string "\n"
;;


