(**Ce module concerne les matrices carrés*)

(** Il sert pour le moment à gérer la matrice de probabilité qu'un joueur soit de perso ..*)

(**Définition du type qui est identique à une matrice mais j'espère qu'on ne mélangera pas les deux*)
type proba_matrice = float array array;;



let make n m (defaut:float)=
  let matrice = Array.make n [|defaut|] in
    for ligne=1 to n-1 do
      matrice.(ligne) <- Array.make m defaut
    done;
    (matrice:proba_matrice)
;;

let dim (mat: proba_matrice)=
  (Array.length mat, Array.length mat.(0)) 
;;

let get (mat: proba_matrice) i j=
  mat.(i).(j)
;;

let m (mat: proba_matrice) i j (new_value:'a)=
    mat.(i).(j) <- new_value
;;

let mm (mat) f i j=
  let val_actuelle = get mat i j in
    m mat i j (f val_actuelle)
;;

let get_norme_ligne mat ligne=
  let somme = ref 0. in
    for col=0 to snd (dim mat) -1 do somme := !somme +. (get mat ligne col) done;
    !somme
;;
 
let get_norme_colonne mat col=
  let somme = ref 0. in
    for ligne=0 to fst (dim mat) -1 do somme := !somme +. (get mat ligne col) done;
    !somme
;;

let norme_colonne mat col norme=
  let norme_actuelle =get_norme_colonne mat col in
    for ligne=0 to snd (dim mat) -1 do mm mat ( ( *.)(norme /. norme_actuelle ) )  ligne col done
;;
    
let norme_ligne mat ligne norme=
  let norme_actuelle = get_norme_ligne mat ligne in
    for col=0 to fst (dim mat) -1 do mm mat ( ( *.) (norme/. norme_actuelle) )   ligne col done
;;

let norme_lignes mat =
    for ligne=0 to snd (dim mat)-1 do norme_ligne mat ligne 1. done
;;

let get_col mat col=
    let n=fst (dim mat) in
    let colonne = Array.make n 0. in
    for ligne=0 to n-1 do colonne.(ligne) <- mat.(ligne).(col) done;
    colonne
;;

let get_ligne mat ligne=
    let n=snd (dim mat) in
    let line = Array.make n 0. in
    for colonne=0 to n-1 do line.(colonne) <- mat.(ligne).(colonne) done;
    ligne
;;

let copy mat=
let (n,m) = dim mat in
let cp = make n m 0. in
for ligne=0 to n-1 do
for col=0 to m-1 do
cp.(ligne).(col) <- mat.(ligne).(col)
done
done;
cp
;;