open Definition

let c_nbjoueurs = 20
;;  
if c_nbjoueurs<Regles.nb_joueurs_min then 
    v_print_string 4 "Arbitre: des erreurs peuvent survenir, le nombre de joueurs est trop faible\n"
;;
let c_whoswho = Array.make c_nbjoueurs Unknown
;;
let c_potions = [|1;1|] (*potion de vie ; poison*)
;;
let id_end = ref (-1)
;;
let id_vote = ref 0
;;

let c_is_dead id = match c_whoswho.(id) with | Mort _-> true |_-> false
;;
let c_is_LG id = match c_whoswho.(id) with | Loup -> true |_ -> false
;;

let conf2j conf = (conf: Joueur3.confiant :> Definition.joueur) (*cette conversion permet la coercion, cest à dire d'indiquer à ocaml que telle sous-classe de joueur sera consideree comme un joueur tout court*)
;; 
let reliable2j reliable = (reliable: Joueur2.reliable :> Definition.joueur)
let jdb j = (j: Joueur.joueur_de_base :> Definition.joueur)
;;

let joueurs = Array.init c_nbjoueurs ( fun i-> if i mod 5<>0 then jdb (new Joueur.joueur_de_base c_nbjoueurs i) else if i mod 7 <>0 then conf2j (new Joueur3.confiant c_nbjoueurs i) else reliable2j (new Joueur2.reliable c_nbjoueurs i) )
;;

let morgue=((Stack.create ()): int Stack.t)
;;


let initialisation () (*ini du jeu: distribue les roles, demande a chaque joueur de s'initialiser en consequence*) =
    let reparti = repartition c_nbjoueurs in 
    v_print_string 2 "Conteur: Repartition des joueurs:\n";
    print_perso_tab reparti;
    print_newline ();
    for id=0 to c_nbjoueurs-1 do 
        joueurs.(id)#donne_info (1,[|id;perso2int reparti.(id)|]);
        c_whoswho.(id) <- reparti.(id);
        let (_,reponse)=joueurs.(id)#pose_question (1,[|id|]) in
        ( v_print 2 "Arbitre: joueur %i  s'identifie comme %i etant un %s ce qui est %b\n" id reponse.(0) (perso2string ( int2perso reponse.(1))) (int2perso reponse.(1)= reparti.(id) && id=reponse.(0)))
        done;
    v_print_string 4 ("Conteur: le jeu commence\n");
    v_print_string 3 "Conteur: Les loups-garous vont se reconnaîtrent\n";
    for id=0 to c_nbjoueurs-1 do
        for id2=id+1 to c_nbjoueurs-1 do
            if c_is_LG id && c_is_LG id2 then (joueurs.(id)#donne_info (1,[|id2;perso2int Loup|]); joueurs.(id2)#donne_info (1,[|id;perso2int Loup|]))
            done
        done;
    v_print_string 3 "Conteur: Les loups-garous se sont reconnus\n"
;;

let is_it_the_end () (*verifie si le jeu est termine*)=
    if array_all ( fun pers->perso_is_dead pers) c_whoswho 
        then (id_end := 0; true) (*tout le monde est mort*) 
        else if array_all (fun pers->pers <> Loup) c_whoswho 
            then (id_end := 1; true) (*tout les loups sont morts*)
            else if array_all (fun pers->pers = Loup || perso_is_dead pers) c_whoswho
                then (id_end := 2; true) (*les loups garous gagnent la partie*)
                else false
;;

let the_end () (*gere la fin du jeu: affiche les gagants, le role de chacun...*)=
    (match !id_end with
        |0 -> v_print_string 3 "Conteur: Tout le monde est mort, le village de Salem s'est entretué !\n"
        |1 -> v_print_string 3 "Conteur: Tous les loups garou sont morts, le village de Salem est sauvé !\n"
        |2 -> v_print_string 3 "Conteur: Tous les villageois sont morts, le village de Salem est tombé aux mains du mal !\n"
        |_ -> v_print_string 4 "Arbitre: Le jeu a quitté pour une raison inconnue"
    );
    v_print_string 3 "Conteur: La partie est terminee\n les roles distribues etaient les suivants\n";
    Array.iteri (fun i-> fun pers -> ( v_print 3 "%i etait %s, de classe %s\n" i (perso2string pers) joueurs.(i)#get_classe)  ) c_whoswho
;;

let nuit () (*gere la nuit: ordre des perso à faire jouer, actions de chacun*)=
    v_print_string 3 "Conteur:La nuit tombe\n";
    v_print_string 3 "Conteur:les loups-garou se reveillent et rodent pendant la nuit\n";
    let (victime,nb_votants)=Definition.appel_au_vote (fun id -> not (c_is_dead id) (*issue 5*) && (c_is_LG id)) (fun (idq,contenu)->c_is_dead (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 3 !id_vote 1 in
    incr id_vote;
    if nb_votants>0 (*issue 10*) 
        then Stack.push victime morgue 
        else v_print_string 4 "Arbitre: il n'y a eu aucun votants pour le vote des loups-garous, la partie devrait être terminée\n";
    v_print_string 3 "Conteur:Les loups-garous se rendorment\n";
    for id=0 to c_nbjoueurs-1 do
        if c_whoswho.(id)=Sorciere
            then 
            begin
                v_print_string 3 "Conteur: la Sorcière se reveille\n";
                let (_,reponse)=joueurs.(id)#pose_question (5,[|victime|]) in
                if reponse.(2)=1 && c_potions.(0)>0
                    then (c_potions.(0)<- c_potions.(0)-1; ignore(Stack.pop morgue);v_print_string 2 "Arbitre: voyante utilise potions vie\n");
                if reponse.(0)=1 && c_potions.(1)>0
                    then (c_potions.(1)<- c_potions.(1)-1; Stack.push reponse.(1) morgue;v_print_string 2 "Arbitre: voyante utilise poison\n");
                v_print_string 3 "Conteur: La sorcière se rendort\n"
            end;
        if c_whoswho.(id)=Voyante
            then (*on pourrait utiliser une procedure de vote un peu speciale pour economiser des lignes de code mais ca serait moins clair*)
            begin
                v_print_string 3 "Conteur: la Voyante se reveille....\n ....et me designe la personne dont elle veut sonder l'identite\n";
                let (_,reponse)= joueurs.(id)#pose_question (4,[||]) in
                match c_whoswho.(reponse.(0)) with
                    |Mort _ -> v_print_string 1 "Arbitre: la Voyante demande l'identite d'un mort, issue13\n"
                    |pers -> (( v_print 2 "Arbitre: La Voyante demande l'identification de %i qui est %s\n" reponse.(0) (perso2string c_whoswho.(reponse.(0)))) ;
                    joueurs.(id)#donne_info (1,[|reponse.(0);perso2int c_whoswho.(reponse.(0))|]));
                v_print_string 3 "Conteur: la voyante se rendort\n"
            end
    done;

;;

let petit_matin ()=
    v_print_string 3 "Conteur: Le jour se leve...\n";
        while not (Stack.is_empty morgue) do
            let id_mort = Stack.pop morgue in
            ( v_print 3 "Conteur: %i est mort cette nuit, %i etait %s\n" id_mort id_mort (perso2string c_whoswho.(id_mort)));
            for id=0 to c_nbjoueurs-1 do (*le conteur informe les joueurs des morts*)
            if not (c_is_dead id) then
                begin
                joueurs.(id)#donne_info (1,[|id_mort;perso2int c_whoswho.(id_mort)|]);
                joueurs.(id)#donne_info (3,[|id_mort;1|])
                end
            done;
            c_whoswho.(id_mort)<- Mort c_whoswho.(id_mort); (*maj des infos du conteur*)
            done
;;

let jour () (*gere le jour: mort des personnages, action specifique, pendaison publique, election d'un maire.../*)=
    v_print_string 3 "Conteur: procedons au vote\n";
    let (suspect,nb_votants)=Definition.appel_au_vote (fun id -> not (c_is_dead id) ) (fun (idq,contenu)->c_is_dead (contenu).(0) (*issue n°6*)) c_nbjoueurs joueurs 2 !id_vote 0 in
    incr id_vote;
    if nb_votants>0 (*issue 10*)
        then begin
            ( v_print 3 "Conteur: %i est donc pendu en place publique\n" suspect);
            ( v_print 3 "Conteur: il revele avant de monter sur l'echafaud qu'il etait %s\n" (perso2string c_whoswho.(suspect)));
            for id=0 to c_nbjoueurs-1 do
                (*le conteur informe les joueurs de lexecution, il faut faire attention  à ne pas informer les joueurs morts ce qui pourrait declencher une exception si l'un d'entre eux utilise par exemple semble_etre_de_mon_cote*)
                if not (c_is_dead id) then
                    begin
                    joueurs.(id)#donne_info (1,[|suspect;perso2int c_whoswho.(suspect)|]);
                    joueurs.(id)#donne_info (3,[|suspect;2|])
                    end
                done;
            c_whoswho.(suspect)<- Mort c_whoswho.(suspect); (*maj des infos du conteur*)
            end
        else v_print_string 4 "Arbitre: personne n'est mort, car il n'ya eu aucun votant, il doit yavoir un problème (cf issue10)\n"
;;


(*Début du jeu*)

initialisation ();

while not (is_it_the_end ()) do 
    nuit ();
    petit_matin ();
    if not (is_it_the_end ()) 
        then jour ();
    flush stdout 
    done
;;
the_end ();;
(*Fin du jeu*)
v_print_string 4 "Conteur: le jeu se termine\n";;

flush stdout;;
if Sys.os_type = "Unix"
then print_float (Sys.time ())
else ignore(Sys.command "pause")
;;
print_string "\n"
;;


