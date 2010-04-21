open Definition

let cause_mort id_mort=
    match id_mort=
    |1->	"tue pendant la nuit"
    |2->	"execute par les villageois"
    |3->	 "abattu par le chasseur qui est mort pendant la nuit"
    |4->	 "abattu par le chasseur qui a été tué par les villageois"
    |5->	 "mort de chagrin au matin (amoureux tué pendant la nuit)"
    |6->	"mort de chagrin le soir (amoureux pendu)"
    |7->	 "mort de chagrin car son amoureux a été abattu"
    |8->	 "abattu car le chasseur est mort de chagrin"


let rec donne_info moi ((id_info,contenu):information) =
    match id_info with
        |0-> Printf.printf "La partie réunit %i joueurs, tu es le numero %i\n" contenu.(0) contenu.(1)
        |1-> Printf.printf "Le joueur %i est en fait %s\n" contenu.(0) (perso2string (int2perso contenu.(1)))
        |2-> assert false
        |3-> Printf.printf "Le joueur %i est mort %s\n" contenu.(0) (cause_mort contenu.(1))
        |4->Printf.printf "Vote num %i, tour %i, le joueur %i a vote contre %i\n" contenu.(0) contenu.(2) contenu.(3) contenu.(4) 
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" moi#get_id)
        |6-> ()
        |7-> Printf.printf "%s amoureux de %s" (if contenu.(0)= moi#get_id then "Tu es" else string_of_int contenu.(0)) (if contenu.(0)<> moi#get_id then "toi" else string_of_int contenu.(1))
        | _ -> ();;



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



class humain c_nbjoueurs numjoueur=
    object (self)
        (**Méthodes principales*)
        inherit Definition.joueur c_nbjoueurs numjoueur
        (**Classe confiant*)
        val classe = "Humain"
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
