(**Joueur 4 dit probabiliste

il utilise une matrice de proba joueur/personnalité afin de s'aider dans ses choix
les matrices doivent le plus possible être normées
il faut encore résoudre le problème concernant le fait que plusieurs joueurs peuvent avoir la même personnalité,  peut etre que le conteur pourrait donner le nombre de joueur ayant telle perso, mais cela rentre-t-il dans les règles ? -> on va dire que oui = nouvelle idi en début de partie
dans ce cas certaines lignes vont être redondantes, il va falloir faire des fonctions qui recherche la proba d'être _un des_ loups garous mais ca devrait pas être trop dur


*)


(**en tête à vérifier souvent si possible*)
(** Nombre de personnalité différentes sans compter qu'il peut y avoir plusieurs de chaque*)
let nb_persos_differentes = 6
(*Loup + Villageois+ Voyante+Cupidon+Chasseur + Sorciere*)

let normalisation_col norme colonne matrice=
    let sum = Array.fold_left (fun sum element-> sum+.element) 0. matrice.(colonne) in
    let m=Array.length matrice.(colonne) in
    for ligne=0 to m-1 do
        matrice.(colonne).(ligne) <- matrice.(colonne).(ligne) /. sum *. norme

(**On veut normaliser une matrice par ses colonnes *)
let normalisation_colonne_mat norme matrice=
    let get_norme_col colonne matrice= Array.fold_left (fun sum element-> sum+.element) 0. matrice.(colonne) in
    let n=Array.length matrice in
    for colonne=0 to n-1 do
        normalisation_col (get_norme_col colonne matrice) colonne matrice
            done
        done
    ;;

let normalisation_ligne norme ligne matrice=
    let sum = (let sum_ =ref 0. in for colonne=0 to Array.length matrice -1 do sum_:=!sum_ +. matrice.(colonne).(ligne) done; !sum_) in
    let m=Array.length matrice in
    for colonne=0 to m-1 do
        matrice.(colonne).(ligne) <- matrice.(colonne).(ligne) /. sum *. norme
        done
;;

(**Idem pour normalisation par  lignes*)
let normalisation_ligne_mat norme (matrice:float array array)=
    let get_norme_ligne ligne matrice= (let sum_ =ref 0. in for colonne=0 to Array.length matrice -1 do sum_:=!sum_ +. matrice.(colonne).(ligne) done; !sum_) in
    let n=Array.length matrice.(0) in (*ca marche car on a une matrice rectangulaire *)
    for ligne=0 to n-1 do
        normalisation_ligne norme ligne (get_norme_ligne ligne matrice) ligne matrice
        done
;;
    
let assimilation_identité moi id identité=
()
;;    

(**Initialisation du joueur en normant les matrices correctement*)
let init moi contenu=
    moi#nb_amoureux <- contenu.(0);
    moi#nb_LGs <- contenu.(1);
    moi#nb_villageois <- contenu.(2);
    moi#nb_voyantes <- contenu.(3);
    moi#nb_sorcieres <- contenu.(4);
    moi#nb_cupidon <- contenu.(5);
    moi#nb_chasseurs <- contenu.(6)
    
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


class probabiliste c_nbjoueurs numjoueur=
  object (self)
    inherit Definition.joueur c_nbjoueurs numjoueur
    val nbjoueurs = c_nbjoueurs
    val id = numjoueur
    val classe = "Probabiliste"
    
    
    val mutable nb_amoureux = 0
    val mutable nb_LGs = 0
    val mutable nb_villageois = 0
    val mutable nb_voyantes = 0
    val mutable nb_sorcieres = 0
    val mutable nb_cupidon = 0
    val mutable nb_chasseurs = 0
    
    
    val proba_perso = Array.make_matrix c_nbjoueurs nb_persos_differentes (1. /. (float_of_int nb_persos_differentes))
    val proba_liaison = new Triang.matrice_triangulaire c_nbjoueurs 0.
    
    
    
    method donne_info = donne_info
    method pose_question = pose_question
  end;;