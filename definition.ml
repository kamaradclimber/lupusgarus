(*contient les definitions de types et les parametres fondamentaux*)

type perso = Unknown |Loup | Villageois
type information=int*(int array);;


let num2perso n=
match n with |0->Unknown |1->Loup|2->Villageois|_->assert false
;;
let perso2num pers=
match pers with |Unknown -> 0 |Loup->1|Villageois->2
;;


(*la classe des joueurs: dans chaque module, le joueur definit sa sous classe avec sa manière propre de repondre aux questions et dassimiler les informations*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    val nbjoueurs = c_nbjoueurs
	val id = numjoueur
	method virtual donne_info : information -> unit
	method virtual pose_question : information -> information
  end;;
 