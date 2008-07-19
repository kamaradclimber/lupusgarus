open Definition

let rec donne_info (id_info,contenu)=
(*pkoi rec ? -> si jamais une information en génère une autre*)
match id_info with
|0 -> (*initialisation*) 
	begin
	()
	end
|_-> ();;


class joueur_de_base c_nbjoueurs numjoueur=
	object
		inherit joueur c_nbjoueurs numjoueur
		val whoswho = Array.make c_nbjoueurs (None : perso option)
		method info = donne_info
		method question = donne_info (*tout a fait temporaire*)
	end;;