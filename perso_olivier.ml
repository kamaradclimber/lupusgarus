open Definition

let tableau_id = Array.make 10000 Unknown ;; (*jai fait un test, cette variable est partagée entre tous te joueurs, c'est un peu de la triche non ? :-) *)

let nb_loups = ref 0 ;;
let nb_villageois = ref 0 ;;(*idem, meme si lutilisation de cette variable nest pas vraiment de la triche !*)

let init objet = (*je comprend pas bien a quoi elle sert: tu reinitialise le nombre de LG à chaque appel de pose_question cest à dire 20000 fois pas partie*)
	print_string "je comprend pas bien a quoi elle sert\n";
	if !nb_loups = 0 
	then nb_loups:= (objet#get_nbjoueurs)/3 ;
	     nb_villageois:= (objet#get_nbjoueurs) - (!nb_loups) ;
		 match objet#who_am_i with
			| Loup ->  tableau_id.(objet#get_id) <- Loup
			| Voyante -> tableau_id.(objet#get_id) <- Voyante
			| Sorciere -> tableau_id.(objet#get_id) <- Sorciere
			|_->() (*pour eviter les warning: this pattern matching is not exhausitive, rajoute une ligne attrape-tout: |_->.....quitte à afficher une erreur*)
;;

let historique_votes = Array.make_matrix 1000 10000 (-1);; (*a la ieme ligne, on lit les votes du ieme tour*)	

let der_vote_lg = ref 0;;

let donne_info objet ((id_info,contenu):information)=
	init objet ;
	match id_info with
	|0 ->(*deprecated*) ()
	|1->tableau_id.(contenu.(0)) <- int2perso contenu.(1) (*ton match with est pas tres comprehensible je le premplace par ce que je crois quil siginifie, sinon il faudrait mettre des begin/with en plus !*)
			(*tu essaye de mettre un entier dans ton tableau_id qui contient des perso, je rajoute donc la fonction int2perso qui permet de faire la conversion*)
	|2-> assert false (*ceci est une réponse de la part des joueurs uniquement*)
	|3-> 	begin (match tableau_id.(contenu.(0)) with 
					|Loup -> nb_loups:=(!nb_loups) - 1
					|_ -> nb_villageois:= (!nb_villageois) - 1); (*tu peux remplacer cette ligne apr la fonction decr nb_villageois, cest plus court !*) 
					tableau_id.(contenu.(0)) <- Mort tableau_id.(contenu.(0)) (*attention Mort nexiste pas tout seul: cest par ex Mort Voyante (cest un type reccursif, mais peut etre que ca evoluera, faudra en discutter)*)
			end (*ya  un truc pas clair avec les points virgules ici: dès que tu mets un point virgule tu sors du match, si tout le bloc pécédent concerne le |_ ->... mets un begin... end*)
	                 
	|4->
		begin 
		( v_print 0 " %i : j'ai bien recu le fait que %i a voté contre %i au vote n°%i (tour: %i) mais je nen fait rien pour le moment\n" objet#get_id contenu.(3) contenu.(4) contenu.(0) contenu.(2)); (*oublie pas les points virgules !*)
		if contenu.(1) = 0 
			then 
				begin
				let compteur = ref 0 in 
				while historique_votes.(!compteur).(contenu.(3)) <>(-1) do (*compteur:=(!compteur)+1*) incr compteur done;
				historique_votes.(!compteur).(contenu.(3))<-contenu.(4) 
				end
			else der_vote_lg:=contenu.(4)
		end
	|5->failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" objet#get_id)
	|_-> ()
;;



let rec pose_question objet ((id_info,contenu):information)=
	init objet ;
	match id_info with
	|0-> (0,[|objet#get_nbjoueurs;objet#get_id|])
	|1-> (1, [|contenu.(0) ; perso2int tableau_id.(contenu.(0))|])
	|2-> 
		begin
		let victime = ref 0 in
		match objet#who_am_i with
			|Loup -> begin
					let liste_copains = ref ([] :int list)in
					for id=((objet#get_nbjoueurs) -1) downto 0 
						do if tableau_id.(id)=Loup then liste_copains :=id::(!liste_copains) done ;
					match (!liste_copains) with 
						|[] -> (while tableau_id.(!victime)=Loup || perso_is_dead tableau_id.(!victime) do victime:= Random.int objet#get_nbjoueurs done ;
								(2,[|!victime|]))
						| h::t -> begin
									if historique_votes.(0).(h) = (-1) (*jai remplacé la match par un if ca marche mieux*)
										then (while tableau_id.(!victime)=Loup || perso_is_dead tableau_id.(!victime) do victime := Random.int objet#get_nbjoueurs done ;
													(2,[|!victime|]))
										else begin let numdervote = ref 0 in while historique_votes.(!numdervote).(h)<>(-1) do numdervote:=(!numdervote)+1 done ; numdervote:=(!numdervote) -1 ;
												if tableau_id.(historique_votes.(!numdervote).(h))<>Loup && not (perso_is_dead tableau_id.(historique_votes.(!numdervote).(h))) 
													then (victime:= historique_votes.(!numdervote).(h) ; (2,[|!victime|]))
	                                                else  (while tableau_id.(!victime)=Loup || perso_is_dead tableau_id.(!victime) do victime:= Random.int objet#get_nbjoueurs done ;
															(2,[|!victime|]))
												end
									end
					end
			|Mort _-> failwith "Le compteur fait voter un mort !!!"
			|_ -> begin
				let id_loup = ref (-1) in   
				for id=((objet#get_nbjoueurs) -1) downto 0 
					do if tableau_id.(id) = Loup then id_loup:=id done ; 
				if !id_loup<>(-1) 
					then (2,[|!id_loup|])
					else let loup1 = ref 0 and loup2 = ref 0 and occurences = ref (-1) in
						for i=0 to ((objet#get_nbjoueurs) -1) 
							do 
							if i<>perso2int (objet#who_am_i) 
								then
								for j=i+1 to ((objet#get_nbjoueurs) -1) 
									do 
									if j<>perso2int (objet#who_am_i) 
									then
									let occ = ref 0 and maxi = ref 0 and maxj = ref 0 in
									while historique_votes.(!maxi).(i) <> (-1) do maxi:= (!maxi)+ 1 done ; 
									maxi:= (!maxi) -1 ;  
									while historique_votes.(!maxj).(j) <> (-1) do maxj:= (!maxj)+ 1 done ; 
									maxj:= (!maxj) -1 ;
									for ki=0 to (!maxi) 
										do
										for kj=0 to (!maxj) 
											do 
											if historique_votes.(kj).(j)=historique_votes.(ki).(i) 
												then occ:=(!occ+1) 
											done; 
										done;
									if (!occ) > (!occurences) 
										then occurences:=(!occ) ; 
									loup1:=i ; 
									loup2:=j (*attention je te lai mis comme ça sera intrprété par ocaml, mais peut etre quil manque un begin ...end*)
									done ; 
							done ;
						victime:= !loup1 ; 
						(2,[|!victime|])
					end
		end
	|3-> if tableau_id.(!der_vote_lg)<>Loup && not (perso_is_dead tableau_id.(!der_vote_lg))
			then (2,[|!der_vote_lg|])
			else let victime = ref (0) in 
				while  tableau_id.(!victime)<>Loup || not (perso_is_dead tableau_id.(!victime))
					do victime:= Random.int objet#get_nbjoueurs done ;
				(2,[|!victime|])
	|4-> let compteur = ref 0 in
			let personne = ref (Random.int objet#get_nbjoueurs) in
			while (!compteur)<100 && tableau_id.(!personne)<>Unknown 
				do compteur:=(!compteur)+1 ; personne := (Random.int objet#get_nbjoueurs) done ; 
			(2,[|!personne|])
	|5-> let (_,[|vict|]) = pose_question objet (2,[||]) in (5,[|1;vict;1|])
	|_-> ((-1),[||]) 
;;



class reliable c_nbjoueurs numjoueur=
object (self)
(*methodes principales*)
inherit joueur c_nbjoueurs numjoueur
val classe = "perso_olivier"
val whoswho = Array.make c_nbjoueurs (Unknown : perso)
method donne_info = donne_info self
method pose_question = pose_question self
(*methodes de modification*)
method mod_whoswho indice nvelle_valeur =whoswho.(indice) <- nvelle_valeur
(*methode d'acces aux infos (get_...) et daffichage (print_.....) *)
method get_whoswho =fun indice-> whoswho.(indice)
method who_am_i = whoswho.(self#get_id)
end;;

