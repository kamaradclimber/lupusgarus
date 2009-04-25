(**Ce module concerne les matrices triangulaires*)

(** Il sert pour le moment à gérer la matrice de probabilité de liaison entre 2 joueurs dans le joueur4*)
(**Les fonction d'accès et de modification vont cependant supprimer la contraintes j<=i *)

(**Définition du type qui est identique à une matrice mais j'espère qu'on ne mélangera pas les deux*)
type 'a matrice_triangulaire = 'a array array;;

(** La représentation d'un matrice triangulaire est :
    
    *
    **
    ***
    ****
    *****
    ******
    ...
    
*) 

let make n (defaut:'a)=
  let matrice = Array.make n [|defaut|] in
    for ligne=1 to n-1 do
      matrice.(ligne) <- Array.make ligne defaut
    done;
    (matrice:'a matrice_triangulaire)
;;

let dim (mat: 'a matrice_triangulaire)=
  Array.length mat
;;

let get (mat: 'a matrice_triangulaire) i j=
  let a= min i j and b=max i j in
  let length = dim mat in
    if a>=length || b>=length || a<0 || b<=0 then failwith (Printf.sprintf "acces à une case inexistante ! case : %i %i" a b);
    Array.unsafe_get (Array.unsafe_get mat b) a
;;

let m (mat: 'a matrice_triangulaire) i j (new_value:'a)=
  let n = Array.length mat and a= min i j and b=max i j in
    if a>=n || b>=n || a<0 || b<0 then failwith "acces à une case inexistante !";
    mat.(b).(a) <- new_value
;;

let mm (mat) f i j=
  let val_actuelle = get mat i j in
    m mat i j (f val_actuelle)
;;

let get_norme_ligne mat ligne=
  let somme = ref 0. in
    for col=ligne to dim mat -1 do somme := !somme +. (get mat ligne col) done;
    !somme
;;
 
let get_norme_colonne mat col=
  let somme = ref 0. in
    for ligne=col to dim mat -1 do somme := !somme +. (get mat ligne col) done;
    !somme
;;

let get_norme mat element=
    let somme = ref 0. in
    for el2=0 to dim mat -1 do somme := !somme +. (get mat element el2) done;
    !somme
;;

let norme mat element=
    let norme_el = get_norme mat element in
    for el2=0 to dim mat -1 do m mat element el2 ((get mat element el2) /. norme_el) done
;;
    

(*let norme_colonne mat col norme=
  let norme_actuelle =get_norme_colonne mat col in
    for ligne=col to dim mat -1 do mm mat ( ( *.)(norme /. norme_actuelle ) )  ligne col done
;;
    
let norme_ligne mat ligne norme=
  let norme_actuelle = get_norme_ligne mat ligne in
    for col=ligne to dim mat -1 do mm mat ( ( *.) (norme/. norme_actuelle) )   ligne col done
;;
*)
