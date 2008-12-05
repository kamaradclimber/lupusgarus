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

(*let analyse_du_vote objet=
    let moi=objet#get_id () in
    recherche des gens de confiance absolue
    les marquer plus
    puis arbres malades sur leur parents en ajoutant plus ou moins et le nombre de amrquage (pour detecter les idiots
;;*)

let assimilation_identité objet id_autre id_identité=
    let identité= int2perso id_identité and ce_que_je_sais = objet#get_whoswho id_autre in
    if ce_que_je_sais <> Unknown && identité <> ce_que_je_sais
        then ( v_print 3 "%i: On me dit que %i est %s, or pour moi il est %s\n" (objet#get_id) id_autre (perso2string identité) (perso2string ce_que_je_sais) );
    objet#mod_whoswho id_autre identité;
    if semble_etre_de_mon_cote objet#who_am_i identité
        then objet#mod_conf id_autre 10
        else objet#mod_conf id_autre (-10)
;;

let assimilation_vote objet id_vote type_vote tour_de_vote votant cible=
    v_print 0 "   %i : j'ai bien recu le fait que %i a vote contre %i au vote n°%i (tour: %i)\n" objet#get_id votant cible id_vote tour_de_vote;
    objet#mod_vote votant cible
    (*insérer ici le code pour l'analyse du vote*)
;;

let gestion_vote objet moment_du_vote=
    match moment_du_vote with
        |0->objet#reset_vote
        |1|2-> ()
        |3 -> ()
        |_ -> Printf.printf "%i: le vote est dit au %i -ieme moment\n" objet#get_id moment_du_vote
;;

let rec donne_info objet ((id_info,contenu):information) =
    match id_info with
        |1-> assimilation_identité objet contenu.(0) contenu.(1)
        |2-> assert false
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" objet#get_id)
        |3-> objet#mod_whoswho contenu.(0) (Mort (objet#get_whoswho contenu.(0)))
        |4-> assimilation_vote objet contenu.(0) contenu.(1) contenu.(2) contenu.(3) contenu.(4)
        |6-> gestion_vote objet contenu.(0)
        | _ -> ();;

let mon_vote objet=
    if objet#who_am_i = Loup 
    then
        let victime=ref (objet#get_id) and count=ref 0 in
        while !count< 100 && (objet#get_whoswho (!victime)=Loup || perso_is_dead (objet#get_whoswho (!victime)) ) do
            victime := Random.int objet#get_nbjoueurs;
            incr count
            done;
        (2,[|!victime|])
    else
        let victime=ref (objet#get_id) and count=ref 0 in
        while !count< 100 && (perso_is_dead (objet#get_whoswho (!victime)) || semble_etre_de_mon_cote objet#who_am_i (objet#get_whoswho  !victime) ) do
            victime := Random.int objet#get_nbjoueurs;
            incr count
            done;
        (2,[|!victime|])
;;

let action_LG objet tour_de_vote=
    if objet#who_am_i = Loup 
        then  
            let victime=ref (objet#get_id) and count=ref 0 in
            while !count< 100 && (objet#get_whoswho (!victime)=Loup || perso_is_dead (objet#get_whoswho (!victime)) ) do
                victime := Random.int objet#get_nbjoueurs;
                incr count
                done;
            (2,[|!victime|]) 
        else 
            failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n vérifier la fonction passée en argument à la procédure de vote " objet#get_id)
;;

let action_voyante objet=
    if objet#get_whoswho (objet#get_id) = Voyante 
        then  (2,[|(Random.int objet#get_nbjoueurs)|]) 
        else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction passe en argument à la procedure de vote " objet#get_id)
;;

let action_sorcière objet victime=
    if victime=objet#get_id then (5,[|0;0;1|]) else (5,[|Random.int 2;Random.int objet#get_nbjoueurs;Random.int 2|])
;;

let rec pose_question objet ((id_info,contenu):information)=
match id_info with
|0-> (0,[|objet#get_nbjoueurs;objet#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (objet#get_whoswho contenu.(0) )|])
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
        method donne_info = donne_info self
        method pose_question = pose_question self
        (*methodes de modification*)
        method mod_whoswho indice nvelle_valeur                     =whoswho.(indice) <- nvelle_valeur
        method mod_conf indice nvelle_valeur                        =conf.(indice) <- max (min nvelle_valeur 10) (-10)
        method mod_vote indice nw                                   = vote.(indice) <-  nw::vote.(indice)
        method reset_vote                                           = for i=0 to c_nbjoueurs -1 do vote.(i) <- [] done
            (*methode d'acces aux infos (get_...) et daffichage (print_.....) *)
        method get_whoswho                                          =fun indice-> whoswho.(indice)
        method  who_am_i                                            = whoswho.(self#get_id)
        method get_conf                                             = conf
        method get_vote                                             = fun i -> vote.(i)
    end;;
