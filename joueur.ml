open Definition

let rec donne_info objet (*pour le moment il est de type 'a puis ce quon en fait va le specifier sans dire quil sagit dun joueur_de_base (car pas encore defini) mais lors de lutilisation il vérifiera la compatibilité ! cf https://mail.google.com/mail/?shva=1#all/11b3af0c2e14abce*) ((id_info,contenu):information)=
(*pkoi rec ? -> si jamais une information en génère une autre... à voir: il faut peut etre distinguer les infos données par le conteur et celle déduite*)
match id_info with
|0 ->(*deprecated*) ()
|1->begin
	let ce_que_je_sais=objet#get_whoswho contenu.(0) in
	if ce_que_je_sais <> Unknown && (int2perso contenu.(1))<> ce_que_je_sais
		then Printf.printf "%i: On me dit que %i est %s, or pour moi il est %s\n" (objet#get_id) (contenu.(0)) (perso2string (int2perso contenu.(1))) (perso2string ce_que_je_sais) ;
		objet#mod_whoswho contenu.(0) (int2perso contenu.(1))
	end
|2-> assert false (*ceci est une réponse de la part des joueurs uniquement*)
|3->objet#mod_whoswho contenu.(0) (Mort (objet#get_whoswho contenu.(0)))
|_-> ();;

let rec pose_question objet ((id_info,contenu):information)=
match id_info with
|0-> (0,[|objet#get_nbjoueurs;objet#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (objet#get_whoswho contenu.(0) )|])
|2-> if objet#who_am_i = Loup 
	then
		let victime=ref (objet#get_id) and count=ref 0 in
		while !count< 100 && (objet#get_whoswho (!victime)=Loup || perso_is_dead (objet#get_whoswho (!victime)) ) do
			victime := Random.int objet#get_nbjoueurs;
			incr count
			done;
		(2,[|!victime|])
	else
		(2,[|(Random.int objet#get_nbjoueurs)|])
|3->if objet#who_am_i = Loup 
	then  
		let victime=ref (objet#get_id) and count=ref 0 in
			while !count< 100 && (objet#get_whoswho (!victime)=Loup || perso_is_dead (objet#get_whoswho (!victime)) ) do
				victime := Random.int objet#get_nbjoueurs;
				incr count
				done;
			(3,[|!victime|]) 
	else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n verifier la fonction passé en argument à la procedure de vote " objet#get_id)
|4->if objet#get_whoswho (objet#get_id) = Voyante 
	then  (2,[|(Random.int objet#get_nbjoueurs)|]) 
	else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction passé en argument à la procedure de vote " objet#get_id)
|5->if contenu.(0)=objet#get_id then (5,[|0;0;1|]) else (5,[|Random.int 2;Random.int objet#get_nbjoueurs;Random.int 2|])
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
		method get_whoswho =fun indice-> whoswho.(indice)
		method who_am_i = whoswho.(self#get_id)
	end;;