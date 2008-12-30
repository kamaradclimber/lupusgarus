open Definition

let semble_etre_de_mon_cote myperso hisperso=
match (myperso,hisperso) with
|Mort _,_->failwith "je suis mort ! (soulevée par la fonction \"semble_etre_de_mon_cote\""
|_, Mort _ -> (v_print_string 4 "Suis-je allie à un mort ? dur de repondre.. ce genre de question ne devrait pas se poser";true)
|Loup, Loup -> true
|Loup, _ -> false
|_, Loup -> false
|_,Unknown -> false
| _,_ -> true
;;

let is_dead moi id = match moi#get_whoswho.(id) with | Mort _-> true |_-> false;;

let is_LG moi id = match moi#get_whoswho.(id) with | Loup -> true |_ -> false;;

let is_unknown moi id = match moi#get_whoswho.(id) with | Unknown -> true |_ -> false;;

let get_participants moi= 
    let participants =
        match moi#get_type_vote with
        | 0 -> Array.make moi#get_nbjoueurs true
        | 1 -> Array.init moi#get_nbjoueurs (fun id -> moi#get_whoswho.(id) = Loup)
        | _ -> (v_print_string 4 "Type de vote inconnu (soulevé par joueur3 dans get_participants)";Array.make moi#get_nbjoueurs true)
    in
    (fun id -> participants.(id))
;;

let analyse_du_vote moi is_participant_au_vote= 
(*Si je suis un loup garou, cette évaluation des votes est inutile tant qu'il n'y a pas d'amoureux.
En effet, je connais tous mes ennemis donc je n'ai pas besoin de savoir qui sont mes ennemis.
En revanche ce système est efficace contre des villageois qui utilisent aussi un sytème de confiance puisque il permet de detecter lesquels sont 'contre moi' et donc d'essayer de les éliminer en priorité, ceci est aussi valable pour détecter la voyante qui m'aurait repéré et voudrait m'éliminer*)
    let marques = Array.make moi#get_nbjoueurs false in
    let bonus_conf = Array.make moi#get_nbjoueurs 0 in
    
    let en_cours = ref (Stack.create () : int Stack.t) and a_traiter = (Stack.create () : int Stack.t) in
    
    while marques <> Array.make moi#get_nbjoueurs true do
        if Stack.is_empty a_traiter 
        then
            (*On remplit la pile des gens à traiter avec 'nos meilleurs amis' non traités *)
            begin
            (*Au début, les meilleurs amis sont les pires...*)
            let best_conf_non_marquée = ref (-10) in
            for ami_potentiel=0 to moi#get_nbjoueurs -1 do
                match marques.(ami_potentiel), (compare moi#get_conf.(ami_potentiel) !best_conf_non_marquée) with
                (*Puis on en découvre d'autres ou des meilleurs*)
                | false,0 -> Stack.push ami_potentiel a_traiter
                | false,1 -> begin best_conf_non_marquée := moi#get_conf.(ami_potentiel) ; Stack.clear a_traiter; Stack.push ami_potentiel a_traiter end
                | _ -> () (*ou pas*)
                done;
            
            (*On aime un peu plus nos 'meilleurs amis' *)
            Stack.iter (fun friend -> moi#mod_conf friend (moi#get_conf.(friend) + 1) ) a_traiter;
            end;
            
        (*On se met au boulot, il faut traiter toute la pile*)
        en_cours := Stack.copy a_traiter ;
        Stack.clear a_traiter;
        while not (Stack.is_empty !en_cours) do
            let désigné = Stack.pop !en_cours in
            if not marques.(désigné) then 
                marques.(désigné) <- true;
                (* Il faut prendre en compte tout ceux qui ont voté contre ce type là*)
                List.iter 
                (fun votant -> 
                    if votant != désigné then (*comme ca on n'interprete pas les votes des idiots qui votent contre eux même [règle n°3]*)
                    begin
                    (*On les aime plus ou moins selon qu'on aime ou non la personne contre laquelle ils ont voté*)
                    bonus_conf.(votant) <- bonus_conf.(votant) + 2 * (compare 0 bonus_conf.(désigné));
                    (* Si en plus ils n'ont pas étés traités, on s'y met*)
                    (*mais pourquoi les y mets on ? on devrait peut etre attendre qu'ils soient nos meilleurs amis ...*)
                    (*il s'agit peut être d'une erreur que d'utiliser un parcours de graphe au lieu d'un parcours de liste, triée dns l'ordre décroissant de la confiance*)
                    (*cependant le sys actuel repose sur la confiance temporaire et indirectement seulement sur la confiance,*)
                    (*donc il est plus 'up-to-date'...*)
                    (*il vaut mieux le laisser comme tel*)
                    if not marques.(votant) then Stack.push votant a_traiter
                    end) moi#get_vote.(désigné)
            done;
        done;
    
    
    (*Enfin, on modifie la confiance en accord avec le vote de tout le monde*)
    Array.iteri (fun id conf_now -> moi#mod_conf id (conf_now + bonus_conf.(id))  ) moi#get_conf;

;;

let assimilation_identité moi id_autre id_identité=
    let identité= int2perso id_identité and ce_que_je_sais = moi#get_whoswho.(id_autre) in
    if ce_que_je_sais <> Unknown && identité <> ce_que_je_sais
        then ( v_print 3 "%i: On me dit que %i est %s, or pour moi il est %s\n" (moi#get_id) id_autre (perso2string identité) (perso2string ce_que_je_sais) );
    moi#mod_whoswho id_autre identité;
    if semble_etre_de_mon_cote moi#who_am_i identité
        then moi#mod_conf id_autre 10
        else moi#mod_conf id_autre (-10)
;;

let assimilation_vote moi id_vote type_vote tour_de_vote votant cible=
    v_print 0 "   %i : j'ai bien recu le fait que %i a vote contre %i au vote n°%i (tour: %i)\n" moi#get_id votant cible id_vote tour_de_vote;
    moi#mod_vote votant cible;
    moi#mod_type_vote type_vote
;;

let fin_du_vote moi = 
    v_print 0 "   %i : voilà mon tableau de confiance : \n" moi#get_id;
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

let rec donne_info moi ((id_info,contenu):information) =
    match id_info with
        |1-> assimilation_identité moi contenu.(0) contenu.(1)
        |2-> assert false
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" moi#get_id)
        |3-> moi#mod_whoswho contenu.(0) (Mort (moi#get_whoswho.(contenu.(0))))
        |4-> assimilation_vote moi contenu.(0) contenu.(1) contenu.(2) contenu.(3) contenu.(4)
        |6-> gestion_vote moi contenu.(0);
        | _ -> ();;

(**Renvoie l'indice de l'élément minimum au sens d'une relation de comparaison vérifiant un prédicat d'un tableau*)
let get_min_array predicat comp tab=
    (**Attention : le prédicat porte sur l'indice du joueur -> utilisation de array_exists_2 sinon gros bug !*)
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


let mon_vote moi=
    let victime = get_min_array (fun id-> not (is_dead moi id) ) (<) moi#get_conf in
    match victime with
    | Some vict -> (2,[|vict|])
    | None -> assert false (**Aucun risque sinon cela signifie que tout le monde est mort*)
;;

let action_LG moi tour_de_vote=
(**c'est presque un doublon de mon_vote, peut etre faut-il la supprimer?*)
    if moi#who_am_i = Loup 
        then  
            let vict = get_min_array (fun id-> not (is_dead moi id) && not (is_LG moi id) ) (<) moi#get_conf in
            match vict with
            | Some victime -> (2,[|victime|]) 
            | None -> assert false (**Aucun risque, il y a toujours des joueurs non-LG vivants sinon c'est la fin de la partie*)
        else 
            failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n vérifier la fonction passée en argument à la procédure de vote " moi#get_id)
;;

let action_voyante moi=
    if moi#get_whoswho.(moi#get_id) = Voyante 
        then  
            begin
            let inconnu = ref (Random.int moi#get_nbjoueurs) and nb_essais =ref 0 in
            while !nb_essais < 100 && is_dead moi !inconnu && is_unknown moi !inconnu do 
                inconnu := Random.int moi#get_nbjoueurs;
                incr nb_essais
                done;
            (2,[|!inconnu|]) 
            end
        else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction passe en argument à la procedure de vote " moi#get_id)
;;

let action_sorcière moi victime=
(*La stratégie de la sorcière est de se sauver elle même et de tuer si possible son pire ennemi si elle le hait vraiment*)
    let vict_potentielle = get_min_array (fun id-> not (is_dead moi id)  ) (<) moi#get_conf in
    match vict_potentielle with
    | None -> assert false
    | Some victime_potentielle -> 
    let empoisonner = (if moi#get_conf.(victime_potentielle) < (-3) then 1 else 0) and sauver = (if victime=moi#get_id then 1 else 0) in
    
    (5,[|empoisonner;victime_potentielle;sauver|])
;;

let rec pose_question moi ((id_info,contenu):information)=
match id_info with
|0-> (0,[|moi#get_nbjoueurs;moi#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (moi#get_whoswho.(contenu.(0)) )|])
|2-> mon_vote moi
|3-> action_LG moi contenu.(0)
|4-> action_voyante moi
|5->action_sorcière moi contenu.(0)
|_-> ((-1),[||])



class confiant c_nbjoueurs numjoueur=
    object (self)
        (**Méthodes principales*)
        inherit Definition.joueur c_nbjoueurs numjoueur
        (**Classe confiant*)
        val classe = "confiant"
        (**Tableau des personnalités connues des autres joueurs*)
        val whoswho = Array.make c_nbjoueurs (Unknown : perso)
        (** Confiance de -10 à +10 envers les autres joueurs*)
        val conf = Array.make c_nbjoueurs 0
        
        (**Stockage temporaire des votes*)
        (** *)
        val vote = Array.make c_nbjoueurs []
        val type_vote = ref 0
        
        (**Méthodes virtuelles héritées*)
        (** *)
        method donne_info = donne_info self
        method pose_question = pose_question self
        
        (**Méthodes de modification*)
        (** *)
        method mod_whoswho indice nvelle_valeur                     = whoswho.(indice) <- nvelle_valeur
        method mod_conf indice nvelle_valeur                        = conf.(indice) <- max (min nvelle_valeur 10) (-10)
        method mod_vote indice nw                                   = vote.(indice) <-  nw::vote.(indice)
        method reset_vote                                           = for i=0 to c_nbjoueurs -1 do vote.(i) <- [] done
        method mod_type_vote nw_type_vote                            = type_vote := nw_type_vote
        
        (**Méthodes d'acces aux infos (get_...) *)
        (** *)
        method get_whoswho                                          = whoswho
        method who_am_i                                             = whoswho.(self#get_id)
        method get_conf                                             = conf
        method get_vote                                             = (vote: int list array)
        method get_type_vote                                        = !type_vote
    end;;
