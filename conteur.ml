(** Ce fichier est le coeur du programme car il représente le maître du jeu qui distribue les rôles, gère les parties et narre l'histoire.
Il peut également servir d'arbitre.
Lui seul connaît les identités des joueurs et peut les appeler pour les informer ou leur poser des questions. Pour celà, il est compilé en dernier afin de ne pas pouvoir être appelé par les autres joueurs (il est le seul à avoir un pointeur vers tous les joueurs
*)

open Definition


(**Définition du nombre de joueurs*)
let c_nbjoueurs = 10
;;  
(**Ce nombre de joueurs ne doit pas être inférieur à une borne définie dans le fichier regles.ml*)
if c_nbjoueurs<Regles.nb_joueurs_min then 
    v_print_string 4 "Arbitre: des erreurs peuvent survenir, le nombre de joueurs est trop faible\n"
;;
(**Tableau contenant un la personnalité de chaque joueur*)
let c_whoswho = Array.make c_nbjoueurs Unknown (*Ce tableau est indexé par l'id des joueurs*)
;;
(** Contient les potions disponibles pour la sorcière
[| potion de vie; poison|]*)
(*Ceci ne sera valide tant qu'il n'y qu'une sorcière*)
let c_potions = [|1;1|] 
;;
(**Contient l'identifiant du type de fin de partie
-1 correspond à une valeur par défaut tant que ce n'est pas la fin de la partie*)
let id_end = ref (-1)
;;
(**Indique le numéro du vote afin de pouvoir s'y reperer*)
let id_vote = ref 0
;;
(**Fonction indiquant si, selon le conteur la personne est morte ou non*)
let c_is_dead id = match c_whoswho.(id) with | Mort _-> true | Amoureux Mort _ -> failwith "j'ai nommé amoureux un mort ce qui est contraire à l'odre normal des évenements !" |_-> false
;;
(**Fonction indiquant si , selon le conteur, la personne est un Loup-Garou vivant ou non*)
let c_is_LG id = match c_whoswho.(id) with | Loup -> true | Amoureux Loup -> true | _ -> false
;;
(**Fonctions assurant les conversions des joueurs en objets de classe Definition.joueur,
cette conversion permet la coercion, cest à dire d'indiquer au vérificateur de type d'ocaml que telle sous-classe de joueur sera consideree exclusivement comme un joueur tout court*)
(** *)
let reliable2j reliable = (reliable: Joueur2.reliable :> Definition.joueur)
let conf2j confiant = (confiant: Joueur3.confiant :> Definition.joueur) 
let jdb j = (j: Joueur.joueur_de_base :> Definition.joueur)

;;
(** Tableau contenant un pointeur vers chaque joueur,
il est ici initialisé avec des joueurs de type confiant*)
let joueurs = Array.init c_nbjoueurs ( fun i-> conf2j (new Joueur3.confiant c_nbjoueurs i))
;;
(** Pile contenant les morts qui ne sont pas encore enregistrés comme morts mais qui ont étés tués.
Il s'agit par exemple de la victime des Loup-Garous ou de la sorcière*)
let morgue=((Stack.create ()): (int*int) Stack.t)
;;
(** Tableau contenant un ordre aléatoire pour affecter une personnalité au hasard à chacun des joueurs
Ce tableau est utilisé lorsque l'on veut parcourir les joueurs afin de ne pas permettre l'identification de la personnalité d'un joueur en regardant le moment où telle personnalité est appelée cf issue14 *)
let ordre = generer_ordre_aleatoire c_nbjoueurs;; 

(**-----------------------------------------------------------------------------------------------*)
(** Début des fonctions définissant les phases de la partie*)

(**Initialisation du jeu: distribue les rôles, demande a chaque joueur de s'initialiser en conséquence*) 
let initialisation () =
    (**Répartition des joueurs*)
    let reparti = repartition c_nbjoueurs in 
    v_print_string 2 "Conteur: Répartition des joueurs:\n";
    print_perso_array reparti;
    
    (*Information des joueurs de leur affectation*)
    for id=0 to c_nbjoueurs-1 do 
        joueurs.(id)#donne_info (1,[|id;perso2int reparti.(id)|]);
        c_whoswho.(id) <- reparti.(id);
        
        (*et vérification qu'ils ont bien compris
        cependant cette vérification n'est pas suivie d'une exception en cas d'erreur*)
        let (_,reponse)=joueurs.(id)#pose_question (1,[|id|]) in
        ( v_print 1 "Arbitre: joueur %i  s'identifie comme %i etant un %s ce qui est %b\n" id reponse.(0) (perso2string ( int2perso reponse.(1))) (int2perso reponse.(1)= reparti.(id) && id=reponse.(0)))
        done;
    
    if verbose <= 4 
    then
        let chaine= "Conteur: le jeu commence" in
        let n = String.length chaine in
        let cadre = String.make (n+2) '-' in
        print_string (cadre^"\n"^chaine^" |\n"^cadre^"\n")
        ;
    (*Cupidon ca désigner les amoureux*)
    v_print_string 3 "Conteur: Cupidon se reveille et décoche une flêche aux deux amoureux\n";
        let cupidon = ref (-1) in
        for id=0 to c_nbjoueurs-1 do 
            if c_whoswho.(id) = Cupidon || c_whoswho.(id) = Amoureux Cupidon then cupidon := id
            done;
        if !cupidon <> -1 (*au cas où il n'y a pas de cupidon*)
            then 
                let  (_, noms) = joueurs.(!cupidon)#pose_question (7,[||]) in
                let am1 = noms.(0) and am2 = noms.(1) in
                if am1 = am2
                then v_print 2 "Arbitre: %i (Cupidon) est pas malin, il désigne %i comme étant amoureux de lui-même, sa flêche ne sera pas prise en compte\n" !cupidon am1
                else begin
                    v_print_string 3 "Je vais prévenir les deux amoureux, ils vont ouvrir les yeux et se reconnaitrent\n";
                    v_print 4 "Conteur: Les amoureux sont %i (%s) et %i (%s)\n" am1 (perso2string c_whoswho.(am1)) am2 (perso2string c_whoswho.(am2));
                    (*On lui dit qu'il est amoureux de untel, il doit alors se mettre à jour, mais ne sais pas qu'elle est l'identité de l'autre*) 
                    joueurs.(am1)#donne_info (7, [|am1;am2|]);
                    joueurs.(am2)#donne_info (7, [|am1;am2|]);
                    
                    (*Le conteur se met à jour*)
                    c_whoswho.(am1) <- Amoureux c_whoswho.(am1);
                    c_whoswho.(am2) <- Amoureux c_whoswho.(am2);
                    v_print_string 3 "Conteur: Les amoureux peuvent désormais se rendormir\n"
                    end;
    (*Les LG se reconnaissent entre eux, c'est à dire qu'on les informe de l'identité des autres LG*)
    v_print_string 3 "Conteur: Les loups-garous vont se reconnaîtrent, ils ouvrent les yeux\n";
    for id=0 to c_nbjoueurs-1 do
        for id2=id+1 to c_nbjoueurs-1 do
            if c_is_LG id && c_is_LG id2 then (joueurs.(id)#donne_info (1,[|id2;perso2int Loup|]); joueurs.(id2)#donne_info (1,[|id;perso2int Loup|]))
            done
        done;
    v_print_string 3 "Conteur: Les loups-garous se sont reconnus, ils se rendorment en se léchant les babines à la pensée des festins futurs\n"
;;


(**Fonction testant si la partie doit se terminer*)
let is_it_the_end () =
(*l'indentation de cette fonction n'est pas canonique mais est plus lisible qu'un escalier*)

    (*tout le monde est mort*)
    if array_all ( fun pers->perso_is_dead pers) c_whoswho                    
        then (id_end := 0; true)
        
    (*tout les loups sont morts*)
    else if array_all (fun pers->not (perso_is_LG pers)) c_whoswho  
        then (id_end := 1; true)
        
    (*les loups garous gagnent la partie*)
    else if array_all (fun pers->perso_is_LG pers || perso_is_dead pers) c_whoswho 
        then (id_end := 2; true)
        
    (*Les amoureux gagnent la partie*)
    else if array_all (fun pers -> perso_is_dead pers || perso_is_amoureux pers) c_whoswho 
        then (id_end := 3; true)
        
    (*la partie n'est pas finie*)
    else false                                                                
;;


(** Fonction gèrant la fin du jeu: affiche les gagnants, le rôle de chacun...*)
let epilogue () =
    (match !id_end with
        |0 -> v_print_string 3 "\nConteur: Tout le monde est mort, le village de Salem s'est entretué !\n"
        |1 -> v_print_string 3 "\nConteur: Tous les loups garou sont morts, le village de Salem est sauvé !\n"
        |2 -> v_print_string 3 "\nConteur: Tous les villageois sont morts, le village de Salem est tombé aux mains du mal !\n"
        |3 -> v_print_string 3 "\nConteur: Seuls les amoureux sont encore vivants, ils vivent heureux, ensemble, pour une longue éternité...\n"
        |_ -> v_print_string 4 "\nArbitre: Le jeu a quitté pour une raison inconnue"
    );
    
    (*Affichage des rôles des participants*)
    if verbose <= 3 
    then
        let chaine= "Conteur: La partie est terminée\n les rôles distribués étaient les suivants" in
        let n = String.length chaine in
        let cadre = String.make (n+2) '-' in
        print_string (cadre^"\n"^chaine^" |\n"^cadre^"\n")
        ;
    
    (*Affichage équipe par équipe*)
    Array.iteri (fun i-> fun perso ->( if perso = Mort Loup || perso = Loup then v_print 3 "%i était %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    v_print_string 3 "------------------\n";
    Array.iteri (fun i-> fun perso ->( if  not (perso = Mort Loup || perso = Loup || perso_is_amoureux perso) then v_print 3 "%i était %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    v_print_string 3 "------------------\n";
    Array.iteri (fun i-> fun perso ->( if  perso_is_amoureux perso then v_print 3 "%i était %s, de classe %s\n" i (perso2string perso) joueurs.(i)#get_classe) ) c_whoswho;
    
;;


let rec autopsie morgue cadavre =
(* définition de la nouvelle morgue: 
il s'agit de stocker en plus la raison du décès et si besoin est, comment on en est arrivé là (par exemple un chasseur amoureux qui meurt de chagrin)
afin de pouvoir adapter les dialogues au mieux
on aura donc un entier qui expliquera tout (cf raisons de la mort dans synatxeechange[...].wiki *)

(*Ce programme doit suggérer plein de question dans les cas anormaux comme: la mort d'un amoureux tué par le chasseur qui est mort de chagrin...
en effet, pour le moment on ne vérifie pas certaines réponses comme le fait que le chasseur ne tue pas son amoureux....,
pour le moment les règles doivent etre suffisament strictes pour que le programmme ne bugge pas mais les affichages ne sont pas forcement optimaux*)

(*Peut etre faudra-t-il indiquer en arguement le moment où l'autopsie est appelée entre jour / petit matin pour mieux adapter l'affichage <- en fait pas besoin car les cas de décès sont suffisament explicites*)
    let (cause_du_deces, id_mort) =  cadavre in
    assert (not (perso_is_dead c_whoswho.(id_mort) ));
    
    (*Affichage de la cause de la mort du joueur*)
    begin
    match cause_du_deces with
        |1-> v_print 3 "Conteur: %i (%s) est mort pendant la nuit, son cadavre est découvert au matin sur le pas de sa porte\n" id_mort (perso2string c_whoswho.(id_mort))
        |2-> v_print 3 "Conteur: %i (%s) est donc pendu en place publique, sa mort est lente et douloureuse et tous les membres du village, en dansant autour\n        de la potence, espèrent avoir éradiqué le mal qui rôde dans le village\n" id_mort (perso2string c_whoswho.(id_mort))
        |3-> v_print 3 "Conteur: Dans son ultime agonie, le chasseur (%i) a pris son fusil et fait un head-shot sur %i (%s)\n" (-1) (*TODO*) id_mort (perso2string c_whoswho.(id_mort))
        |4-> v_print 3 "Conteur: Lorsque les villageois sont venus chercher le chasseur (%i), celui-ci a dégainé son fusil et abattu %i (%s)\n" (-1) (*TODO*) id_mort (perso2string c_whoswho.(id_mort))
        |5-> v_print 3 "Conteur: Lorsque %i (%s) a découvert le cadavre de son amoureux %i, une grande tristesse s'est emparée de lui et il va mettre fin à ses jours seul dans la forêt\n" id_mort (perso2string c_whoswho.(id_mort)) (-1) (*TODO*)
        |6-> v_print 3 "Conteur: Après que %i aie été pendu par l'ensemble du village, %i (%s) s'est retiré, seul, dans la forêt et nepouvant vivre sans %i a mis fin à ses jours\n" (-1) (*TODO*) id_mort (perso2string c_whoswho.(id_mort)) (-1) (*TODO*)
        |7-> v_print 3 "Conteur: %i ayant été abattu par le chasseur, %i (%s) se fane de chagrin et se retire dans du monde discretement dans la forêt\n" (-1) (*TODO*) id_mort (perso2string c_whoswho.(id_mort))
        |8-> v_print 3 "Conteur: Avant d'aller se suicider, %i a sorti son fusil et exprimé son désespoir en tirant sur %i (%s)\n" (-1) (*TODO*) id_mort (perso2string c_whoswho.(id_mort))
        |_ -> failwith (Printf.sprintf "%i est mort pour une raison inconnue (soulevé par autopsie dans conteur.ml)" id_mort)
    end;
    (*le conteur informe les joueurs des morts*)
    for id=0 to c_nbjoueurs-1 do 
    if not (c_is_dead id) then
        begin
        joueurs.(id)#donne_info (1,[|id_mort;perso2int c_whoswho.(id_mort)|]);
        joueurs.(id)#donne_info (3,[|id_mort;cause_du_deces|]);
        v_print_string 0 "information dun joueur\n"
        end
    done;
    (*maj des infos du conteur*)
    c_whoswho.(id_mort)<- Mort c_whoswho.(id_mort);

    (*Maintenant on gère les cas particuliers de mort*)
    if perso_is_amoureux c_whoswho.(id_mort) && (match cause_du_deces with 5|6|7->false|_->true)
        then begin
            (**Recherche de l'autre amoureux*)
            let ame_soeur = ref (-1) in
            for id=0 to c_nbjoueurs-1 do
                if perso_is_amoureux c_whoswho.(id) && id <> id_mort then ame_soeur := id
                done;        
            assert (!ame_soeur <> (-1));
            (*On a désormais l'amoureux, on va le tuer en ajoutant la cause de mort en fonction de celle de l'autre amoureux*)
            (*On regarde d'abord si l'amoureux est déjà dans la morgue*)
            stack_filter (fun (c,id)-> id <> !ame_soeur) morgue;
            match cause_du_deces with
                |1-> Stack.push (5,!ame_soeur) morgue
                |2-> Stack.push (6,!ame_soeur) morgue
                |3|4-> Stack.push (7,!ame_soeur) morgue
                |5|6|7|8-> failwith "Si on en arrive là c'est que l'autre amoureux est mort car son amoureux est mort"
                |_-> failwith "type de mort inconnu ! (cf autopsie dans conteur.ml)"
            end;
            (*on met des point-virgule entre les tests si jamais il y a plusieurs cas qui se présentent par exemple chasseur amoureux*)
    if perso_is_chasseur c_whoswho.(id_mort)
        then begin
            let (_,reponse)= joueurs.(id_mort)#pose_question (2,[||]) in
            let id_gibier = reponse.(0) in
            stack_filter (fun (c,id)-> id <> id_gibier) morgue;
            match cause_du_deces with
                |1-> Stack.push (3,id_gibier) morgue
                |2-> Stack.push (4,id_gibier) morgue
                |5|6-> Stack.push (8,id_gibier) morgue
                |3|4-> failwith "Si on en arrive là c'est qu'il y a deux chasseur ou un gros bug"
                |7|8 -> failwith "Si on en arrive là cest que le chasseur a tué son amoureux ou alors il y a plusieurs chasseur ou un gros bug"
                |_-> failwith "type de mort inconnu ! (cf autopsie dans conteur.ml"
            end
;;

(**Fonction gérant la nuit: ordre des perso à faire jouer, actions de chacun*)
let nuit () =
    v_print_string 3 "Conteur:La nuit tombe\n";
    
    (*Réveil des LG*)
    v_print_string 3 "Conteur:les loups-garous se réveillent et rodent pendant la nuit\n";
    let (victime, nb_votants)=
        appel_au_vote 
            (fun id -> not (c_is_dead id) && (c_is_LG id))            (* Définition des votants*)
            (fun (_,contenu)->c_is_dead (contenu).(0) )               (* Définition d'un vote invalide*)
            c_nbjoueurs                                               (* Nombre de joueurs dans la partie*)
            joueurs                                                   (* Tableau des joueurs*)
            3                                                         (* Type de la question : idq *)
            !id_vote
            1                                                         (* Type de vote : de jour, de nuit...*)
        in
    
    (* Il y a eu un vote de plus: on incrémente le nombre de vote*)
    incr id_vote;
    
    (* Test pour prendre ou non le vote en compte*)
    if nb_votants>0 (*issue 10*) 
        then Stack.push (1,victime) morgue 
        else v_print_string 4 "Arbitre: il n'y a eu aucun votants pour le vote des loups-garous, la partie devrait être terminée\n";


    v_print_string 3 "Conteur:Les loups-garous se rendorment\n";

    (*Réveil des joueurs qui jouent la nuit*)
    let id = ref 0 in
    for i=0 to c_nbjoueurs - 1 do
        id := ordre.(i);
        
        (* C'est au tour de la sorcière de jouer*)
        if c_whoswho.(!id) = Sorciere || c_whoswho.(!id) = Amoureux Sorciere
            then 
                begin
                v_print_string 3 "Conteur: la Sorcière se réveille\n";
                let (_,reponse) = joueurs.(!id)#pose_question (5,[|victime|]) in
                
                (*Vérification de la validité de la réponse de la sorcière et prise en compte de sa décision*)
                if reponse.(2)=1 && c_potions.(0) > 0
                    then 
                        begin
                        c_potions.(0)<- c_potions.(0) - 1; 
                        ignore(Stack.pop morgue);
                        v_print_string 2 "Arbitre: Sorcière utilise potions vie\n"
                        end;
                if reponse.(0)=1 && c_potions.(1) > 0
                    then 
                        begin
                        c_potions.(1)<- c_potions.(1) - 1; (*on enlève de toute facon meme si la cible est morte [règle n°7] *)
                        if not (c_is_dead reponse.(1))
                            then (Stack.push (1,reponse.(1)) morgue; v_print 2 "Arbitre: Sorcière utilise poison contre %i\n" reponse.(1))
                            else v_print 2 "Arbitre: Sorcière (%i) utilise poison contre %i qui est mort, une potion lui est retirée [règle n°7]\n" !id reponse.(1);
                        end;
                        
                v_print_string 3 "Conteur: La sorcière se rendort\n"
                end;

        (*C'est au tour de la voyante de jouer*)
        if c_whoswho.(!id)=Voyante || c_whoswho.(!id)=Amoureux Voyante
            then (*on pourrait utiliser une procedure de vote un peu speciale pour économiser des lignes de code mais ca serait moins clair*)
                begin
                v_print_string 3 "Conteur: la Voyante se reveille.. ....et me designe la personne dont elle veut sonder l'identité\n";
                let (_,reponse)= joueurs.(!id)#pose_question (4,[||]) in
                match c_whoswho.(reponse.(0)) with
                    | Unknown -> assert false
                    | Mort _ -> v_print_string 1 "Arbitre: la Voyante demande l'identité d'un mort, issue13\n"
                    | pers -> 
                        begin
                        v_print 2 "Arbitre: La Voyante demande l'identification de %i qui est %s\n" reponse.(0) (perso2string c_whoswho.(reponse.(0))) ;
                        joueurs.(!id)#donne_info (1,[|reponse.(0);perso2int c_whoswho.(reponse.(0))|])
                        end;
                v_print_string 3 "Conteur: la voyante se rendort\n"
                end
    done

;;


(** Fonction gérant le lever du soleil (et oui c'est compliqué !) c'est à dire annoncer les morts et officialiser les décès*)
let petit_matin ()=
    v_print_string 3 "Conteur: Le jour se lève...\n";
    while not (Stack.is_empty morgue) do
        let cadavre = Stack.pop morgue in
        autopsie morgue cadavre
        done
;;


(**Fonction gérant le jour: morts des personnages, action specifique, pendaison publique.../*)
let jour () =
    v_print_string 3 "Conteur: procédons au vote\n";
    let (suspect,nb_votants)=appel_au_vote (fun id -> not (c_is_dead id) ) (fun (idq,contenu)->c_is_dead (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 2 !id_vote 0 in
    incr id_vote;
    if nb_votants>0 (*issue 10*)
        then begin
            Stack.push (2,suspect) morgue;
            while not (Stack.is_empty morgue) do
                let cadavre=Stack.pop morgue in
                autopsie morgue cadavre
                done
            end
        else v_print_string 4 "Arbitre: personne n'est mort, car il n'ya eu aucun votant, il doit yavoir un problème (cf issue10)\n"
;;


(**Début du jeu*)
initialisation ();

(**Déroulement de la partie*)
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
else ignore(Sys.command "pause")
;;
print_string "\n"
;;


