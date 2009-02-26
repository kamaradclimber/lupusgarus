(**Ce module concerne les matrices triangulaires*)

(** Il sert pour le moment � g�rer la matrice de probabilit� de liaison entre 2 joueurs dans le joueur4*)
(**Les fonction d'acc�s et de modification vont cependant supprimer la contraintes j<=i *)

(**D�finition du type qui est identique � une matrice mais j'esp�re qu'on ne m�langera pas les deux*)
type 'a matrice_triangulaire_t = 'a array array;;

(** La repr�sentation d'un matrice triangulaire est :

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
        matrice.(ligne) <- Array.make (ligne+1) defaut
        done;
    (matrice:'a matrice_triangulaire_t)
    ;;
    
let get (mat: 'a matrice_triangulaire_t) length i j=
    let a= min i j and b=max i j in
    if a>=length || b>=length || a<0 || b<0 then failwith "acces � une case inexistante !";
    Array.unsafe_get (Array.unsafe_get mat b) a
    ;;

let m (mat: 'a matrice_triangulaire_t) i j (new_value:'a)=
    let n = Array.length mat and a= min i j and b=max i j in
    if a>=n || b>=n || a<0 || b<0 then failwith "acces � une case inexistante !";
    mat.(b).(a) <- new_value
    ;;
    
class ['a] matrice_triangulaire n (defaut:'a)=
    object(self)
    val contenu = make n defaut
    method length = Array.length contenu
    method get =get contenu (self#length)
    method m = m contenu
    end
    ;;