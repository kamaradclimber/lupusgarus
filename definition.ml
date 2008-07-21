(*contient les definitions de types et les parametres fondamentaux*)

type perso = Unknown |Loup | Villageois
type information=int*(int array);;

Random.init (int_of_float (Unix.time ()));;



let int2perso n=
	match n with |0->Unknown |1->Loup|2->Villageois|_->assert false
;;
let perso2int pers=
	match pers with |Unknown -> 0 |Loup->1|Villageois->2
;;
let perso2string pers=
	match pers with |Unknown -> "Unknown" |Loup->"Loup"|Villageois->"Villageois"
;;
let print_perso_tab tab=
	Array.iter (fun x->Printf.printf "%s " (perso2string x) ) tab;print_newline ();;

(*la classe des joueurs: dans chaque module, le joueur definit sa sous classe avec sa manière propre de repondre aux questions et dassimiler les informations*)

class virtual joueur c_nbjoueurs numjoueur=
  object (self)
    val nbjoueurs = c_nbjoueurs
	val id = numjoueur
	method virtual donne_info : information -> unit
	method virtual pose_question : information -> information
	method get_id = (id:int)
	method get_nbjoueurs = (nbjoueurs:int)
  end;;
 
let repartition nbjoueurs=
	let rep=Array.init nbjoueurs (fun i->i) in
	let perm tab i1 i2= let tmp=tab.(i1) in tab.(i1)<-tab.(i2); tab.(i2)<-tmp in
	for i=0 to 2*nbjoueurs do perm rep (Random.int nbjoueurs) (Random.int nbjoueurs) done;
	(*on a desormais un tableau ãléatoire avec les id des joueurs*)
	let rep2=Array.make nbjoueurs Villageois in
	for i=0 to nbjoueurs-1 do 
		match i mod 3 with
			|0->rep2.(i)<- Loup
			|_->()
		done;
	let rep3=Array.make nbjoueurs Unknown in
	for i=0 to nbjoueurs-1 do rep3.(rep.(i))<-rep2.(i) done;
	rep3
	;;
	