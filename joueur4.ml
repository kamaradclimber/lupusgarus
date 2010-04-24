open Definition

let cause_mort id_mort=
    match id_mort with
    |1->	"tue pendant la nuit"
    |2->	"execute par les villageois"
    |3->	 "abattu par le chasseur qui est mort pendant la nuit"
    |4->	 "abattu par le chasseur qui a été tué par les villageois"
    |5->	 "mort de chagrin au matin (amoureux tué pendant la nuit)"
    |6->	"mort de chagrin le soir (amoureux pendu)"
    |7->	 "mort de chagrin car son amoureux a été abattu"
    |8->	 "abattu car le chasseur est mort de chagrin"
    |_ -> "pour une autre raison"


let rec donne_info moi ((id_info,contenu):information) =
    match id_info with
        |0-> Printf.printf "La partie réunit %i joueurs, tu es le numero %i\n" contenu.(0) contenu.(1)
        |1-> Printf.printf "Le joueur %i est en fait %s\n" contenu.(0) (perso2string (int2perso contenu.(1)))
        |2-> assert false
        |3-> Printf.printf "Le joueur %i est mort %s\n" contenu.(0) (cause_mort contenu.(1))
        |4->Printf.printf "Vote num %i, tour %i, le joueur %i a vote contre %i\n" contenu.(0) contenu.(2) contenu.(3) contenu.(4) 
        |5-> failwith (Printf.sprintf "%i: j'ai recu une info concernant le conteur, il y a erreur\n" moi#get_id)
        |6-> ()
        |7-> Printf.printf "%s amoureux de %s" (if contenu.(0)= moi#get_id then "Tu es" else string_of_int contenu.(0)) (if contenu.(0)<> moi#get_id then "toi" else string_of_int contenu.(1))
        | _ -> ();;



let idq1 untel=
    Printf.printf "Donne moi l'identite de %i selon toi\n" untel;
    flush stdout;
    Scanf.fscanf stdin "%s\n" (fun s->string2perso s)
    ;;

let idq2 ()=
    Printf.printf "Qui veux-tu tuer ?\n";
    flush stdout;
    
    Scanf.fscanf stdin  "%s\n" (fun i->int_of_string i)
    ;;

let idq3 ()=
    Printf.printf "Tu es LG qui veux tu devorer ?\n";
    idq2 ()
    ;;

let idq4 ()=
    Printf.printf "Conteur : Tu es la voyante, quelle identite veux tu connaitre ?\n";
    flush stdout;
    Scanf.fscanf  stdin "%i\n" (fun i->i)
    ;;

let idq5 victime=
    Printf.printf "Conteur : Sorciere, veux tu tuer un individu ? [oui/non]\n";
    flush stdout;
    let qui=ref 0 in
    let tuer = Scanf.fscanf  stdin "%s\n" (fun s->match String.lowercase s with "oui"->1 |_->0) in
    if tuer=1 then (Printf.printf "qui veux tu tuer ? \n"; flush stdout; qui := Scanf.fscanf  stdin "%i\n" (fun i->i));
    Printf.printf "La victime des LG est pour le moment %i, veux tu la sauver ? [oui/non]\n" victime;
    flush stdout;
    let sauver = Scanf.fscanf  stdin "%s\n" (fun s->match String.lowercase s with "oui"->1 |_->0) in
    
    [|tuer;!qui;sauver|]

let rec pose_question moi ((id_info,contenu):information)=
match id_info with
|0-> (0,[|moi#get_nbjoueurs;moi#get_id|])
|1-> (1, [|contenu.(0) ; perso2int (idq1 contenu.(0) )|])
|2-> (2, [|idq2 ()|] )
|3-> (2, [|idq3 () |])
|4-> (2, [|idq4 () |])
|5-> (5, idq5 contenu.(0) )
|_-> ((-1),[||])



class humain c_nbjoueurs numjoueur=
    object (self)
        (**Méthodes principales*)
        inherit Definition.joueur c_nbjoueurs numjoueur
        (**Classe confiant*)
        val classe = "Humain"
        
        (**Méthodes virtuelles héritées*)
        (** *)
        method donne_info = donne_info self
        method pose_question = pose_question self
        
       
    end;;
