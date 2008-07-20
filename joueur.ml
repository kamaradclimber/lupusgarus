open Definition

let rec donne_info objet (*pour le moment il est de type 'a puis ce quon en fait va le specifier sans dire quil sagit dun joueur_de_base (car pas encore defini) mais lors de lutilisation il vérifiera la compatibilité ! cf https://mail.google.com/mail/?shva=1#all/11b3af0c2e14abce*) ((id_info,contenu):information)=
(*pkoi rec ? -> si jamais une information en génère une autre... à voir: il faut peut etre distinguer les infos données par le conteur et celle déduite*)
match id_info with
|0 -> (*initialisation*) 
	begin
	objet#mod_whoswho 0 Unknown (*temporaire*)
	end
|_-> ();;

let rec pose_question objet ((id_info,contenu):information)=
match id_info with
|0-> (0,[|objet#get_id ;objet#get_nbjoueurs|])
|1-> (1, [|perso2num (objet#get_whoswho contenu.(0) )|])
|_-> ((-1),[||])


class joueur_de_base c_nbjoueurs numjoueur=
	object (self)
		(*methodes principales*)
		inherit joueur c_nbjoueurs numjoueur
		val whoswho = Array.make c_nbjoueurs (Unknown : perso)
		method donne_info = donne_info self
		method pose_question = pose_question self
		(*methodes de modification*)
		method mod_whoswho indice nvelle_valeur =whoswho.(indice) <- nvelle_valeur
		(*methode d'acces aux infos (get_...) et daffichage (print_.....) *)
		method get_id= id
		method get_nbjoueurs=nbjoueurs
		method get_whoswho =fun indice-> whoswho.(indice)
	end;;