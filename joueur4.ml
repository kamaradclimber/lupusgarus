(**Joueur 4 dit probabiliste

il utilise une matrice de proba joueur/personnalité afin de s'aider dans ses choix
il utilise également une matrice triangulaire qui stocke *non pas la proba que des joueurs soient amoureux* mais plutot la proba 
qu'ils soient dans la meme équipe -> cest à dire qu'il est une amélioration du joueur de confiance

Une matrice se présente sous la forme suivante: (l'ordre est un exemple)

                probaLG probaVillageois probaCupidon
joueur1
joueur2
joueur3

On y accède par matrice.(joueur).(personnalité)

La somme de chaque ligne fait 1, la somme de chaque colonne est _libre_
Lorsqu'une proba est à 0 ou à 1, elle est inmodifiable. (cf methodes m_proba_perso et mm_proba_perso)
L'analyse des proba de liaison et de perso sont très disjointes et ne suivent donc pas les mêmes règles.
On assume qu'il y n'y a qu'une paire d'amoureux au plus

*)



(**en tête à vérifier souvent si possible*)
(** attention à enlever au fur et à mesure les appels au whoswho s'il est décidé de ne pas s'en servir*)

(* Début du fichier *)

(**ne pas ouvrir les fichiers contenant les primitives sur les matrices carrés et triangulaires, ils utilisent le meme espace de nom !*)

open Definition
;;

let assimilation_identité moi id identité=
    (*On va mettre 1 à la proba du joueur et 0 partout ailleurs*)
    for personnalite=0 to moi#get_nbjoueurs-1 do
        moi#m_proba_perso id personnalite 0.
        done;
    moi#m_proba_perso id identité 1.;
    
    
    (* puis si tous les joueurs de ce type sont trouvés, réduire à 0 la proba qu'un autre joueur soit de ce type*)
    (*et on itère le processus jusqu'à ce que la matrice ne bouge plus ce qui devrait arriver assez vite*)
    let ancienne_proba = ref (Carre.make 1 1 0.) in
    while !ancienne_proba != moi#proba_perso do
        ancienne_proba := (Carre.copy moi#proba_perso);
        (*Il faudrait mesurer si cette seconde partie est utilisée de temps en temps...*)
        let norme_col = Carre.get_norme_colonne moi#proba_perso identité in
        let nb_of_1 = Array.fold_right (fun x sum_of_1->if x=1. then sum_of_1+1 else sum_of_1) (Carre.get_col moi#proba_perso identité) 0 in
        if (float_of_int nb_of_1) = norme_col
            then
                for ligne=0 to moi#get_nbjoueurs-1 do if moi#proba_perso.(ligne).(identité) <> 1. then moi#m_proba_perso ligne identité 0. done
        ;
        moi#norme_lignes (* <- ceci permet de trouver d'eventuelles personnalités par déduction*)
        done;
;;    

(**Ceci est la fonction la plus complexe du joueur puisqu'elle fixe la facon dont le joueur va comprendre ce qui se passe dans le jeu
il faudrait aussi travailler sur les autres fonctions qui récupèrent des informations afin de compléter le travail*)
let assimilation_vote moi id_vote type_vote tour_de_vote votant cible=
(* Les idées sont les suivantes:
    deux personnes qui votent l'une contre l'autre sont moins probablement liées
    deux personnes qui votent pareil sont peut etre liées
    *)
 ()
;;

let fin_du_vote moi=
  ()
;;

let gestion_vote moi moment_du_vote=
  ()
;;

(**On suppose qu'il y n'y a qu'une paire d'amoureux au maximum pour le moment*)
let coup_de_foudre moi am1 am2=
    let n= moi#get_nbjoueurs in
    for id=0 to n-1 do
        for id2=0 to id-1 do
            moi#m_proba_liaison id2 id 0.
            done
        done;
    moi#m_proba_liaison am1 am2 1.
;;

let init moi contenu=
    let nb_personnalites = Array.fold_right (fun sum x->sum+x) contenu 0 - contenu.(0) in (*il est probable qu'un bug soit ici, je ne comprend plus bien la syntaxe de fold_right*)
    moi#m_nb_personnalites nb_personnalites;
    moi#init nb_personnalites;
    moi#m_nb_amoureux  contenu.(0);
    moi#m_nb_LGs  contenu.(1);
    moi#m_nb_villageois  contenu.(2);
    moi#m_nb_voyantes  contenu.(3);
    moi#m_nb_sorcieres  contenu.(4);
    moi#m_nb_cupidon  contenu.(5);
    moi#m_nb_chasseurs  contenu.(6)
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
    val classe = "Probabiliste"
    
    val mutable nb_personnalites =0
    method m_nb_personnalites n = nb_personnalites <- n
    val proba_perso = (Carre.make c_nbjoueurs 37 0.)
    val proba_liaison = Triang.make c_nbjoueurs (1./. (float_of_int c_nbjoueurs))
    
    val mutable nb_amoureux = 0
    method m_nb_amoureux n = (nb_amoureux <- n; if n>2 then v_print 4 "Avertissement : il semble y avoir trop d'amoureux (%i)\n" n)
    val mutable nb_LGs = 0
    method m_nb_LGs n = nb_LGs <- n
    val mutable nb_villageois = 0
    method m_nb_villageois n = nb_villageois <- n
    val mutable nb_voyantes = 0
    method m_nb_voyantes n = nb_voyantes <- n
    val mutable nb_sorcieres = 0
    method m_nb_sorcieres n = nb_sorcieres <- n
    val mutable nb_cupidon = 0
    method m_nb_cupidon n = nb_cupidon <- n
    val mutable nb_chasseurs = 0    
    method m_nb_chasseurs n = nb_chasseurs <- n
    
    method proba_perso = proba_perso
    method proba_liaison = proba_liaison
    method m_proba_perso i j new_value= if self#proba_perso.(i).(j) != 0. && self#proba_perso.(i).(j) != 1. then Carre.m proba_perso i j new_value
    method mm_proba_perso f i j= if self#proba_perso.(i).(j) != 0. && self#proba_perso.(i).(j) != 1. then Carre.mm proba_perso f i j 
    method m_proba_liaison = Triang.m proba_liaison
    method mm_proba_liaison = Triang.mm proba_liaison
    
    method norme_lignes = Carre.norme_lignes proba_perso
    
    method init nb_personnalites= for i=0 to self#get_nbjoueurs-1 do proba_perso.(i) <- Array.make nb_personnalites (1./. (float_of_int nb_personnalites)) done
    method get_nb_personnalites = nb_personnalites
    method get_whoswho = (Printf.printf "usage whoswho deprecated chez joueur4\n"; [||])
    method mod_whoswho _ _ =(Printf.printf "usage whoswho deprecated chez joueur4\n")
    
    (**Cette methode est fiable, bien que probabiliste, car on ne peut modifier un 1 ou un 0 dans le tableau des probas *)
    method who_am_i = int2perso (Definition.array_max_i proba_perso.(self#get_id) (<))
    
    method donne_info = donne_info self
    method pose_question = pose_question self
  end;;
