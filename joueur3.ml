open Definition

let semble_etre_de_mon_cote myperso hisperso=
match (myperso,hisperso) with
|Mort _,_->failwith "je suis mort ! (soulev�e par la fonction \"semble_etre_de_mon_cote\""
|_, Mort _ -> (v_print_string 4 "Suis-je alli� � un mort ? dur de r�pondre.. ce genre de question ne devrait pas se poser";true)
|Loup, Loup -> true
|Loup, _ -> false
|_, Loup -> false
|_,Unknown -> false
| _,_ -> true
;;

let rec donne_info objet ((id_info,contenu):information)=
(*pkoi rec ? -> si jamais une information en g�n�re une autre... � voir: il faut peut etre distinguer les infos donn�es par le conteur et celle d�duite*)
match id_info with
|0 ->(*deprecated*) ()
|1->begin
	let nb_autre = contenu.(0) and perso_autre= int2perso contenu.(1) in
	let ce_que_je_sais = objet#get_whoswho contenu.(0) in
	if ce_que_je_sais <> Unknown && perso_autre <> ce_que_je_sais
		then ( v_print 3 "%i: On me dit que %i est %s, or pour moi il est %s\n" (objet#get_id) nb_autre (perso2string perso_autre) (perso2string ce_que_je_sais) );
	objet#mod_whoswho nb_autre perso_autre;
	if semble_etre_de_mon_cote objet#who_am_i perso_autre
		then objet#mod_conf nb_autre 10
		else objet#mod_conf nb_autre (-10)
	end
|2-> assert false (*ceci est une r�ponse de la part des joueurs uniquement*)
|3->objet#mod_whoswho contenu.(0) (Mort (objet#get_whoswho contenu.(0)))
|4->( v_print 0 "   %i : j'ai bien recu le fait que %i a vot� contre %i au vote n�%i (tour: %i) mais je nen fait rien pour le moment\n" objet#get_id contenu.(3) contenu.(4) contenu.(0) contenu.(2))
|5->failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" objet#get_id)
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
		let victime=ref (objet#get_id) and count=ref 0 in
		while !count< 100 && (perso_is_dead (objet#get_whoswho (!victime)) || semble_etre_de_mon_cote objet#who_am_i (objet#get_whoswho  !victime) ) do
			victime := Random.int objet#get_nbjoueurs;
			incr count
			done;
		(2,[|!victime|])
|3->if objet#who_am_i = Loup 
	then  
		let victime=ref (objet#get_id) and count=ref 0 in
			while !count< 100 && (objet#get_whoswho (!victime)=Loup || perso_is_dead (objet#get_whoswho (!victime)) ) do
				victime := Random.int objet#get_nbjoueurs;
				incr count
				done;
			(3,[|!victime|]) 
	else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas un loup garou, une telle erreur n'aurait pas du arriver\n verifier la fonction pass� en argument � la procedure de vote " objet#get_id)
|4->if objet#get_whoswho (objet#get_id) = Voyante 
	then  (2,[|(Random.int objet#get_nbjoueurs)|]) 
	else failwith (Printf.sprintf "%i dit: ERREUR je ne suis pas la voyante, une telle erreur n'aurait pas du arriver\n verifier la fonction pass� en argument � la procedure de vote " objet#get_id)
|5->if contenu.(0)=objet#get_id then (5,[|0;0;1|]) else (5,[|Random.int 2;Random.int objet#get_nbjoueurs;Random.int 2|])
|_-> ((-1),[||])



class probabiliste c_nbjoueurs numjoueur=
	object (self)
		(*methodes principales*)
		inherit Definition.joueur c_nbjoueurs numjoueur
		val classe = "probabiliste"
		val whoswho = Array.make c_nbjoueurs (Unknown : perso)
		val conf = Array.make c_nbjoueurs 0 (*de -10 � +10 *)
		method donne_info = donne_info self
		method pose_question = pose_question self
		(*methodes de modification*)
		method mod_whoswho indice nvelle_valeur =whoswho.(indice) <- nvelle_valeur
		method mod_conf indice nvelle_valeur =conf.(indice) <- max (min nvelle_valeur 10) (-10)
		(*methode d'acces aux infos (get_...) et daffichage (print_.....) *)
		method get_whoswho =fun indice-> whoswho.(indice)
		method  who_am_i = whoswho.(self#get_id)
	end;;