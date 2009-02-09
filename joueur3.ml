open Definition

let rec semble_etre_de_mon_cote myperso hisperso=
match (myperso,hisperso) with
|Amoureux _, Amoureux _ -> true
|Amoureux perso, _ |_, Amoureux perso -> false (*Les deux cas peuvent arriver (penser � cupidon qui sait qui sont les amoureux sans forc�ment en �tre un)
avec ce syst�me un amoureux va consid�rer tout de suite tout le monde comme son ennemi et donc va voter contre les gens sens� �tre dans son �quipe donc se faire reperer, il faudrait donc subtiliser un peu le traitement de cette fonction*) 
|Mort _,_->failwith "je suis mort ! (soulev�e par la fonction \"semble_etre_de_mon_cote\""
|_, Mort _ -> (v_print_string 4 "Suis-je alli� � un mort ? d�r de repondre.. ce genre de question ne devrait pas se poser";true)
|Loup, Loup -> true
|Loup, _ -> false
|_, Loup -> false
|_,Unknown -> false
| _,_ -> true
;;

let is_dead moi id = match moi#get_whoswho.(id) with | Mort _-> true |_-> false;;

let is_LG moi id = match moi#get_whoswho.(id) with | Loup| Amoureux Loup -> true |_ -> false;;

let is_unknown moi id = match moi#get_whoswho.(id) with | Unknown|Amoureux Unknown -> true |_ -> false;;

let is_amoureux moi id = match moi#get_whoswho.(id) with |Amoureux _ -> true | Mort Amoureux _ -> assert false (*ca ne devrait pas arriver sinon voir pkoi *)
|_-> false;;

let get_participants moi= 
    let participants =
        match moi#get_type_vote with
        | 0 -> Array.make moi#get_nbjoueurs true
        | 1 -> Array.init moi#get_nbjoueurs (fun id -> moi#get_whoswho.(id) = Loup)
        | _ -> (v_print_string 4 "Type de vote inconnu (soulev� par joueur3 dans get_participants)";Array.make moi#get_nbjoueurs true)
    in
    (fun id -> participants.(id))
;;

let analyse_du_vote moi is_participant_au_vote= 
(*Si je suis un loup garou, cette �valuation des votes est inutile tant qu'il n'y a pas d'amoureux.
En effet, je connais tous mes ennemis donc je n'ai pas besoin de savoir qui sont mes ennemis.
En revanche ce syst�me est efficace contre des villageois qui utilisent aussi un syt�me de confiance puisque il permet de detecter lesquels sont 'contre moi' et donc d'essayer de les �liminer en priorit�, ceci est aussi valable pour d�tecter la voyante qui m'aurait rep�r� et voudrait m'�liminer*)
    let marques = Array.make moi#get_nbjoueurs false in
    let bonus_conf = Array.make moi#get_nbjoueurs 0 in
    
    let en_cours = ref (Stack.create () : int Stack.t) and a_traiter = (Stack.create () : int Stack.t) in
    
    while marques <> Array.make moi#get_nbjoueurs true do
        if Stack.is_empty a_traiter 
        then
            (*On remplit la pile des gens � traiter avec 'nos meilleurs amis' non trait�s *)
            begin
            (*Au d�but, les meilleurs amis sont les pires...*)
            let best_conf_non_marqu�e = ref (-10) in
            for ami_potentiel=0 to moi#get_nbjoueurs -1 do
                match marques.(ami_potentiel), (compare moi#get_conf.(ami_potentiel) !best_conf_non_marqu�e) with
                (*Puis on en d�couvre d'autres ou des meilleurs*)
                | false,0 -> Stack.push ami_potentiel a_traiter
                | false,1 -> begin best_conf_non_marqu�e := moi#get_conf.(ami_potentiel) ; Stack.clear a_traiter; Stack.push ami_potentiel a_traiter end
                | _ -> () (*ou pas*)
                done;
            
            (*On aime un peu plus nos 'meilleurs amis' *)
            Stack.iter (fun friend -> moi#mod_conf friend (moi#get_conf.(friend) + 1) ) a_traiter;
            end;
            
        (*On se met au boulot, il faut traiter toute la pile*)
        en_cours := Stack.copy a_traiter ;
        Stack.clear a_traiter;
        while not (Stack.is_empty !en_cours) do
            let d�sign� = Stack.pop !en_cours in
            if not marques.(d�sign�) then 
                marques.(d�sign�) <- true;
                (* Il faut prendre en compte tout ceux qui ont vot� contre ce type l�*)
                List.iter 
                (fun votant -> 
                    if votant != d�sign� then (*comme ca on n'interprete pas les votes des idiots qui votent contre eux m�me [r�gle n�3]*)
                    begin
                    (*On les aime plus ou moins selon qu'on aime ou non la personne contre laquelle ils ont vot�*)
                    bonus_conf.(votant) <- bonus_conf.(votant) + 2 * (compare 0 bonus_conf.(d�sign�));
                    (* Si en plus ils n'ont pas �t�s trait�s, on s'y met*)
                    (*mais pourquoi les y mets on ? on devrait peut etre attendre qu'ils soient nos meilleurs amis ...*)
                    (*il s'agit peut �tre d'une erreur que d'utiliser un parcours de graphe au lieu d'un parcours de liste, tri�e dns l'ordre d�croissant de la confiance*)
                    (*cependant le sys actuel repose sur la confiance temporaire et indirectement seulement sur la confiance,*)
                    (*donc il est plus 'up-to-date'...*)
                    (*il vaut mieux le laisser comme tel*)
                    if not marques.(votant) then Stack.push votant a_traiter
                    end) moi#get_vote.(d�sign�)
            done;
        done;
    
    
    (*Enfin, on modifie la confiance en accord avec le vote de tout le monde*)
    Array.iteri (fun id conf_now -> moi#mod_conf id (conf_now + bonus_conf.(id))  ) moi#get_conf;

;;

(**Permet au joueur d'int�grer la personnalit� d'un joueur et modifier sa confiance en lui en cons�quence*)
(*TODO pourrait �tre grandement factoris�e*)
let assimilation_identit� moi id_autre id_identit�=
    let identit�= int2perso id_identit� and ce_que_je_sais = moi#get_whoswho.(id_autre) in
    if ce_que_je_sais <> Unknown && ce_que_je_sais <> Amoureux Unknown  && identit� <> ce_que_je_sais && not (is_amoureux moi id_autre)  
        then ( v_print 3 "%i: On me dit que %i est %s, or pour moi il est %s\n" (moi#get_id) id_autre (perso2string identit�) (perso2string ce_que_je_sais) );
    if ce_que_je_sais = Amoureux Unknown
        then moi#mod_whoswho id_autre (Amoureux identit�)
        else moi#mod_whoswho id_autre identit�
    ;
    match moi#who_am_i with 
        |Amoureux ma_perso ->
            begin
                (*je suis amoureux mais l'autre ?*)
                if (match ce_que_je_sais with  Amoureux Unknown->true |_->false)
                    then 
                        (* nous sommes amoureux l'un de l'autre*)
                        moi#mod_conf id_autre 10 (*mais en th�orie, la conf en lui est d�j� max*)
                    else
                        (* ce n'est pas mon amoureux donc je me demande s'il est avec moi ou non*)
                        if semble_etre_de_mon_cote ma_perso identit�
                            then moi#mod_conf id_autre 5
                            else moi#mod_conf id_autre (-10)
            end
        |_->
            begin 
                (*je ne suis pas amoureux et l'autre ?*)
                if (match ce_que_je_sais with  Amoureux Unknown->true |_->false)
                    then 
                        (*je suis donc cupidon et pas amoureux et l'autre est donc un ennemi*)
                        moi#mod_conf id_autre (-10)
                    else
                        (*personne n'est amoureux donc c'est le cas normal*)
                        if semble_etre_de_mon_cote moi#who_am_i identit�
                            then moi#mod_conf id_autre 10
                            else moi#mod_conf id_autre (-10)
            end
;;

let assimilation_vote moi id_vote type_vote tour_de_vote votant cible=
    v_print 0 "   %i : j'ai bien recu le fait que %i a vote contre %i au vote n�%i (tour: %i)\n" moi#get_id votant cible id_vote tour_de_vote;
    moi#mod_vote votant cible;
    moi#mod_type_vote type_vote
;;

let fin_du_vote moi = 
    v_print 0 "   %i : voil� mon tableau de confiance : \n" moi#get_id;
    print_int_array  0 moi#get_conf
;;



let gestion_vote moi moment_du_vote=
    let is_participant = (get_participants moi) in
    match moment_du_vote with
        |0->moi#reset_vote
        |1|2-> (analyse_du_vote moi is_participant);moi#reset_vote
        |3 -> fin_du_vote moi
        |_ -> Printf.printf "%i: le vote est dit au %i -ieme moment, ca me parait bizarre\n" moi#get_id moment_du_vote
;;

(**Indique au joueur qu'il est amoureux de qq1*)
let coup_de_foudre moi am1 am2=
    let autre = 
    (if moi#get_id <> am1
    then
        (*(assert moi#get_id = am2; (*on ne transmet cette information qu'aux amoureux, donc je suis obligatoirement l'un deux*)*)
        am1
    else
        am2
    )
    in
    (*Modification des status*)
    moi#mod_whoswho autre (Amoureux moi#get_whoswho.(autre));
    moi#mod_whoswho moi#get_id (Amoureux moi#get_whoswho.(moi#get_id));
    for id=0 to moi#get_nbjoueurs-1 do moi#mod_conf id (-10) done;
    moi#mod_conf autre (10);
    moi#mod_conf moi#get_id (10)
;;

let rec donne_info moi ((id_info,contenu):information) =
    match id_info with
        |1-> assimilation_identit� moi contenu.(0) contenu.(1)
        |2-> assert false
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" moi#get_id)
        |3-> moi#mod_whoswho contenu.(0) (Mort (moi#get_whoswho.(contenu.(0))))
        |4-> assimilation_vote moi contenu.(0) contenu.(1) contenu.(2) contenu.(3) contenu.(4)
        |6-> gestion_vote moi contenu.(0);
        |7->coup_de_foudre moi contenu.(0) contenu.(1)
        | _ -> ();;

(**Renvoie l'indice de l'�l�ment minimum au sens d'une relation de comparaison v�rifiant un pr�dicat d'un tableau*)
let get_min_array predicat comp tab=
    (**Attention : le pr�dicat porte sur l'indice du joueur -> utilisation de array_exists_2 sinon gros bug !*)
    if array_exists_2 predicat tab
     then 
        begin
            let i_best = ref 0 in
            for i=0 to Array.length tab -1 do
                if (comp tab.(i) tab.(!i_best) && predicat i && predicat !i_best) || (predicat i && not (predicat !i_best) ) then i_best := i
                done;
            Some !i_best
        end
    else
        None
;;

(**Renvoie l'indice de l'�l�ment minimum v�rifiant un pr�dicat d'un tableau;
s'il y en a plusieurs alors le hasard d�cide*)
let get_min_array_2 predicat tab=
(*Cette fonction est illisible, les commentaires expliquent tout*)
(*On recherche s'il y au moins un indice qui v�rifie le pr�dicat*)
    if array_exists_2 predicat tab 
        then
            (*Si oui on cherche lequel*)
            let au_moins_un = ref 0 in
            while not (predicat !au_moins_un) do incr au_moins_un done;
            (*on stocke la liste des indices les plus petits pour le moment*)
            let iminl=ref [!au_moins_un]  in
            (*Puis on parcourt le tableau � la recherche des autres*)
            for i=(!au_moins_un +1 ) to Array.length tab-1 do
                if predicat i then
                    (* et on les rajoute � la liste des minimums voire on la remplace si on trouve quelquechose de plus petit*)
                    match compare tab.(List.hd !iminl) tab.(i) with
                        |0-> iminl := (i::(!iminl))
                        |1->iminl:=[i]
                        |_-> ()
                done;
            (* enfin on choisit au hasard*)
            let tmp=Array.of_list !iminl in let n=Array.length tmp in 
            let choix=tmp.(Random.int n) (*si �galit�, le hasard decide*) in
            Some choix
        else
            None
;;

let mon_vote moi=
    let victime = get_min_array_2 (fun id-> not (is_dead moi id) ) moi#get_conf in
    match victime with
    | Some vict -> (2,[|vict|])
    | None -> assert false (**Aucun risque sinon cela signifie que tout le monde est mort*)
;;

let action_LG moi tour_de_vote=
(**c'est presque un doublon de mon_vote, peut etre faut-il la supprimer?*)
    if is_LG moi moi#get_id 
        then  
            let vict = get_min_array_2 (fun id-> not (is_dead moi id) && not (is_LG moi id) ) moi#get_conf in
            match vict with
            | Some victime -> (2,[|victime|]) 
            | None -> assert false (**Aucun risque, il y a toujours des joueurs non-LG vivants sinon c'est la fin de la partie*)
        else 
            failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n v�rifier la fonction pass�e en argument � la proc�dure de vote " moi#get_id)
;;

let action_voyante moi=
    if moi#who_am_i = Voyante || moi#who_am_i = Amoureux Voyante
        then  
            begin
            let inconnu = ref (Random.int moi#get_nbjoueurs) and nb_essais =ref 0 in
            while !nb_essais < 100 && is_dead moi !inconnu && is_unknown moi !inconnu do 
                inconnu := Random.int moi#get_nbjoueurs;
                incr nb_essais
                done;
            (2,[|!inconnu|]) 
            end
        else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction passe en argument � la procedure de vote " moi#get_id)
;;

let action_sorci�re moi victime=
(*La strat�gie de la sorci�re est de se sauver elle m�me et de tuer si possible son pire ennemi si elle le hait vraiment*)
    let vict_potentielle = get_min_array_2 (fun id-> not (is_dead moi id)  ) moi#get_conf in
    match vict_potentielle with
    | None -> assert false
    | Some victime_potentielle -> 
    let empoisonner = (if moi#get_conf.(victime_potentielle) < (-3) then 1 else 0) and sauver = (if victime=moi#get_id then 1 else 0) in
    
    (5,[|empoisonner;victime_potentielle;sauver|])
;;

(**d�signe les deux amoureux et met� jour la confiance concernant les amoureux*)
let tir_a_larc moi=
    let am1 = Random.int moi#get_nbjoueurs in
    let am2 = ref am1 in
    (*On s'assure que les amoureux sont bien distincts*)
    while !am2 = am1 do am2 := Random.int moi#get_nbjoueurs done;
    moi#mod_whoswho am1 (Amoureux moi#get_whoswho.(am1));
    moi#mod_whoswho !am2 (Amoureux moi#get_whoswho.(!am2));
    (*On met � jour la confiance*)
    moi#mod_conf am1 (-10);
    moi#mod_conf !am2 (-10);
    (* Si jamais cupidon s'est d�sign� lui meme ceci risque de poser un probl�me car il va detester son amoureux mais en sera inform� juste apr�s par le conteur et modifera la confiance dans le bon sens � ce moment l�*)
    (7, [|am1; !am2|])
;;

let rec pose_question moi ((id_info,contenu):information)=
match id_info with
|0-> (0,[|moi#get_nbjoueurs;moi#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (moi#get_whoswho.(contenu.(0)) )|])
|2-> mon_vote moi
|3-> action_LG moi contenu.(0)
|4-> action_voyante moi
|5-> action_sorci�re moi contenu.(0)
|6-> failwith "idq6 non attribu�e"
|7-> tir_a_larc moi
|_-> ((-1),[||])



class confiant c_nbjoueurs numjoueur=
    object (self)
        (**M�thodes principales*)
        inherit Definition.joueur c_nbjoueurs numjoueur
        (**Classe confiant*)
        val classe = "confiant"
        (**Tableau des personnalit�s connues des autres joueurs*)
        val whoswho = Array.make c_nbjoueurs (Unknown : perso)
        (** Confiance de -10 � +10 envers les autres joueurs*)
        val conf = Array.make c_nbjoueurs 0
        
        (**Stockage temporaire des votes*)
        (** *)
        val vote = Array.make c_nbjoueurs []
        val type_vote = ref 0
        
        (**M�thodes virtuelles h�rit�es*)
        (** *)
        method donne_info = donne_info self
        method pose_question = pose_question self
        
        (**M�thodes de modification*)
        (** *)
        method mod_whoswho indice nvelle_valeur                     = whoswho.(indice) <- nvelle_valeur
        method mod_conf indice nvelle_valeur                        = conf.(indice) <- max (min nvelle_valeur 10) (-10)
        method mod_vote indice nw                                   = vote.(indice) <-  nw::vote.(indice)
        method reset_vote                                           = for i=0 to c_nbjoueurs -1 do vote.(i) <- [] done
        method mod_type_vote nw_type_vote                            = type_vote := nw_type_vote
        
        (**M�thodes d'acces aux infos (get_...) *)
        (** *)
        method get_whoswho                                          = whoswho
        method who_am_i                                             = whoswho.(self#get_id)
        method get_conf                                             = conf
        method get_vote                                             = (vote: int list array)
        method get_type_vote                                        = !type_vote
    end;;
