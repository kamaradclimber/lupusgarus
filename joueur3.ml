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

let is_dead objet id = match objet#get_whoswho.(id) with | Mort _-> true |_-> false

let get_participants objet=
	let participants =
		match objet#get_type_vote with
		| 0 -> Array.make objet#get_nbjoueurs true
		| 1 -> Array.init objet#get_nbjoueurs (fun id -> objet#get_whoswho.(id) = Loup)
		| _ -> (v_print_string 4 "Type de vote inconnu (soulevé par joueur3 dans get_participants)";Array.make objet#get_nbjoueurs true)
	in
	fun id -> participants.(id)
;;

let analyse_du_vote objet is_participant_au_vote=
	
    let get_amis pred = (*détermine les joueurs vérifiant un certain prédicat dans le tableau conf*)
        begin
        let candidates = ref [] in
        for id=0 to objet#get_nbjoueurs -1 do
            if pred objet#get_conf.(id) then candidates := id :: !candidates
            done;   
        !candidates
        end in

	(*On va déterminer chaque fois que nécessairela liste des meilleurs amis, c'est à dire ceux qui sont qualifiés de la best_conf *)
    let best_friends = ref [] and best_conf = ref 10 in
	
	(*Initialisation des tableaux des gens dont les votes contre eux ont déjà étés analysés*)
    let bonus_conf = Array.make objet#get_nbjoueurs 0 and marques = Array.init objet#get_nbjoueurs (fun id -> not (is_participant_au_vote id) || is_dead objet id) in
	
	(*Algorithme des arbres malades*)
    while marques <> Array.make objet#get_nbjoueurs true do
        let a_traiter = (Stack.create () : int Stack.t) in
	
        while List.length (List.filter (fun id -> not (is_dead objet id) ) !best_friends) = 0 do 
			best_friends := get_amis ( (=) !best_conf ); 
			decr best_conf 
			done;
        (*On a désormais une liste avec les meilleurs amis marqués ou non, sachant que la confiance diminue à chaque fois*)
		
		(*les amis qui sont non marqués sont ajoutés dans la liste des gens à traiter et on les honore d'un +1 dans la confiance temporaire*)
        List.iter (fun id ->if not marques.(id) then (Stack.push id a_traiter; bonus_conf.(id) <- bonus_conf.(id) +1)) !best_friends;
        
		(*Enfin on traite tout les gens à traiter !*)
        while not (Stack.is_empty a_traiter) do
            let a_examiner = Stack.pop a_traiter in
            List.iter (
                fun votant ->
					let modifiant = compare 0 bonus_conf.(a_examiner) in
					Stack.push votant a_traiter;
					bonus_conf.(votant) <- bonus_conf.(votant) + modifiant
                ) objet#get_vote.(a_examiner);
            marques.(a_examiner) <- true
            done
        done;
	Definition.print_int_tab bonus_conf;
	Array.iteri (fun id-> objet#mod_conf (id + bonus_conf.(id))) objet#get_conf
;;

let assimilation_identité objet id_autre id_identité=
    let identité= int2perso id_identité and ce_que_je_sais = objet#get_whoswho.(id_autre) in
    if ce_que_je_sais <> Unknown && identité <> ce_que_je_sais
        then ( v_print 3 "%i: On me dit que %i est %s, or pour moi il est %s\n" (objet#get_id) id_autre (perso2string identité) (perso2string ce_que_je_sais) );
    objet#mod_whoswho id_autre identité;
    if semble_etre_de_mon_cote objet#who_am_i identité
        then objet#mod_conf id_autre 10
        else objet#mod_conf id_autre (-10)
;;

let assimilation_vote objet id_vote type_vote tour_de_vote votant cible=
    v_print 0 "   %i : j'ai bien recu le fait que %i a vote contre %i au vote n°%i (tour: %i)\n" objet#get_id votant cible id_vote tour_de_vote;
    objet#mod_vote votant cible;
	objet#mod_type_vote type_vote
;;

let gestion_vote objet moment_du_vote=
	let is_participant = get_participants objet in
    match moment_du_vote with
        |0->objet#reset_vote
        |1|2-> analyse_du_vote objet is_participant; objet#reset_vote
        |3 -> ()
        |_ -> Printf.printf "%i: le vote est dit au %i -ieme moment, ca me parait bizarre\n" objet#get_id moment_du_vote
;;

let rec donne_info objet ((id_info,contenu):information) =
    match id_info with
        |1-> assimilation_identité objet contenu.(0) contenu.(1)
        |2-> assert false
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" objet#get_id)
        |3-> objet#mod_whoswho contenu.(0) (Mort (objet#get_whoswho.(contenu.(0))))
        |4-> assimilation_vote objet contenu.(0) contenu.(1) contenu.(2) contenu.(3) contenu.(4)
        |6-> gestion_vote objet contenu.(0)
        | _ -> ();;

let mon_vote objet=
    if objet#who_am_i = Loup 
    then
        let victime=ref (objet#get_id) and count=ref 0 in
        while !count< 100 && ( (objet#get_whoswho.(!victime) )=Loup || is_dead objet !victime)  do
            victime := Random.int objet#get_nbjoueurs;
            incr count
            done;
        (2,[|!victime|])
    else
        let victime=ref (objet#get_id) and count=ref 0 in
        while !count< 100 && ((is_dead objet !victime) || semble_etre_de_mon_cote objet#who_am_i (objet#get_whoswho.( !victime)) ) do
            victime := Random.int objet#get_nbjoueurs;
            incr count
            done;
        (2,[|!victime|])
;;

let action_LG objet tour_de_vote=
    if objet#who_am_i = Loup 
        then  
            let victime=ref (objet#get_id) and count=ref 0 in
            while !count< 100 && ((objet#get_whoswho.(!victime) = Loup || is_dead objet !victime) ) do
                victime := Random.int objet#get_nbjoueurs;
                incr count
                done;
            (2,[|!victime|]) 
        else 
            failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n vérifier la fonction passée en argument à la procédure de vote " objet#get_id)
;;

let action_voyante objet=
    if objet#get_whoswho.(objet#get_id) = Voyante 
        then  (2,[|(Random.int objet#get_nbjoueurs)|]) 
        else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction passe en argument à la procedure de vote " objet#get_id)
;;

let action_sorcière objet victime=
    if victime=objet#get_id then (5,[|0;0;1|]) else (5,[|Random.int 2;Random.int objet#get_nbjoueurs;Random.int 2|])
;;

let rec pose_question objet ((id_info,contenu):information)=
match id_info with
|0-> (0,[|objet#get_nbjoueurs;objet#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (objet#get_whoswho.(contenu.(0)) )|])
|2-> mon_vote objet
|3-> action_LG objet contenu.(0)
|4-> action_voyante objet
|5->action_sorcière objet contenu.(0)
|_-> ((-1),[||])



class confiant c_nbjoueurs numjoueur=
    object (self)
        (*methodes principales*)
        inherit Definition.joueur c_nbjoueurs numjoueur
        val classe = "confiant"
        val whoswho = Array.make c_nbjoueurs (Unknown : perso)
        val conf = Array.make c_nbjoueurs 0 (*de -10 à +10 *)
        val vote = Array.make c_nbjoueurs []
		val type_vote = ref 0
        method donne_info = donne_info self
        method pose_question = pose_question self
        (*methodes de modification*)
        method mod_whoswho indice nvelle_valeur                     = whoswho.(indice) <- nvelle_valeur
        method mod_conf indice nvelle_valeur                        = conf.(indice) <- max (min nvelle_valeur 10) (-10)
        method mod_vote indice nw                                   = vote.(indice) <-  nw::vote.(indice)
        method reset_vote                                           = for i=0 to c_nbjoueurs -1 do vote.(i) <- [] done
		method mod_type_vote nw_type_vote							= type_vote := nw_type_vote
            (*methode d'acces aux infos (get_...) et daffichage (print_.....) *)
        method get_whoswho                                          = whoswho
        method  who_am_i                                            = whoswho.(self#get_id)
        method get_conf                                             = conf
        method get_vote                                             = vote
		method get_type_vote										= !type_vote
    end;;
