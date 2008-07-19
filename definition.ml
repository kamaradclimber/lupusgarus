(*contient les definitions de types et les parametres fondamentaux*)

type perso = Loup | Villageois



let num2perso n=
match n with |1->Loup|2->Villageois|_->assert false
;;
let perso2num pers=
match pers with |Loup->1|Villageois->2
;;


(*la classe des joueurs: dans chaque module, le joueur definit sa sous classe avec sa maniere propre de repondre aux questions et dassimiler les informations*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    val nbjoueurs = c_nbjoueurs
	val id = numjoueur
	method virtual info : (int*int array)-> unit
	method virtual question : (int*int array)-> unit
  end;;