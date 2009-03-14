(**Joueur 4 dit probabiliste

il utilise une matrice de proba joueur/personnalité afin de s'aider dans ses choix
les matrices doivent le plus possible être normées, comme ce n'est pas possible (preuve à faire)
on va normer seulement les lignes)

*)


(**en tête à vérifier souvent si possible*)
(** attention à enlever au fur et à mesure les appels au whoswho si il est décidé de ne pas s'en servir*)

(* Début du fichier *)

(**ne pas ouvrir les fichiers contenant les primitives sur les matrices carrés et triangulaires, ils utilisent le meme espace de nom !*)

open Definition
;;

let assimilation_identité moi id identité=
()
;;    

let assimilation_vote moi id_vote type_vote tour_de_vote votant cible=
  ()
;;

let fin_du_vote moi=
  ()
;;

let gestion_vote moi moment_du_vote=
  ()
;;

let coup_de_foudre moi am1 am2=
()
;;

let init moi contenu=
    let nb_personnalites = Array.fold_right (fun sum x->sum+x) contenu 0 - contenu.(0) in
    moi#init nb_personnalites
;;

let rec donne_info moi ((id_info,contenu):information) =
    match id_info with
        |1-> assimilation_identité moi contenu.(0) contenu.(1)
        |2-> assert false
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" moi#get_id)
        |3-> moi#mod_whoswho contenu.(0) (Mort (moi#get_whoswho.(contenu.(0))))
        |4-> assimilation_vote moi contenu.(0) contenu.(1) contenu.(2) contenu.(3) contenu.(4)
        |6-> gestion_vote moi contenu.(0);
        |7->coup_de_foudre moi contenu.(0) contenu.(1)
        |8-> init moi contenu
        | _ -> ();;

let mon_vote moi=
  (2,[|0|])
;;

let action_LG moi tour_de_vote=
  (2,[|0|])
;;

let action_voyante moi=
  (2,[|0|])
;;

let action_sorcière moi victime=
  (5,[|0;0;0|])
;;

let tir_a_larc moi=
  (7, [|0;1|])
;;

let rec pose_question moi ((id_info,contenu):information)=
    match id_info with
    |0-> (0,[|moi#get_nbjoueurs;moi#get_id|])
    |1-> (1, [|contenu.(0) ; perso2int (moi#get_whoswho.(contenu.(0)) )|])
    |2-> mon_vote moi
    |3-> action_LG moi contenu.(0)
    |4-> action_voyante moi
    |5-> action_sorcière moi contenu.(0)
    |6-> failwith "idq6 non attribu\130e"
    |7-> tir_a_larc moi
    |_-> ((-1),[||])
;;



class probabiliste c_nbjoueurs numjoueur=
  object (self)
    inherit Definition.joueur c_nbjoueurs numjoueur
    (*val nbjoueurs = c_nbjoueurs
    val id = numjoueur*)
    val classe = "Probabiliste"
    
        
    val proba_perso = (Carre.make c_nbjoueurs 37 0.)
    val proba_liaison = Triang.make c_nbjoueurs 0.
    
    method proba_perso = proba_perso
    method proba_liaison = proba_liaison
    method m_proba_perso = Carre.m proba_perso
    method mm_proba_perso = Carre.mm proba_perso
    method m_proba_liaison = Triang.m proba_liaison
    method mm_proba_liaison = Triang.mm proba_liaison
    
    method init nb_personnalites= for i=0 to self#get_nbjoueurs-1 do proba_perso.(i) <- Array.make nb_personnalites (1./. (float_of_int nb_personnalites)) done
    
    method get_whoswho = (Printf.printf "usage whoswho deprecated chez joueur4\n"; [||])
    method mod_whoswho _ _ =(Printf.printf "usage whoswho deprecated chez joueur4\n")
    
    method donne_info = donne_info self
    method pose_question = pose_question self
  end;;
