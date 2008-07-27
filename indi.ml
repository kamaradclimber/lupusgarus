(*les fonctions sans rapport mais indispensables*)
Random.init (int_of_float (Unix.time ()));;


let pause ()=
	(*inutile : Sys.command "pause" sous windows*)
	Sys.catch_break true;
	Printf.printf "\n%s\n" "on fait la pause, ctrl+c pour reprendre";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend"
;;

let print_int_tab tab= Array.iter (fun x->Printf.printf "%i " x) tab;print_newline ();;

let vote_majorite (resultats:int array)=
	let imaxl=ref [0] and sum=ref 0 in
	for i=1 to Array.length resultats-1 do (*le fait de commencer � 1 causera un bug s'il n'y a qun joueur*)
		sum:=!sum+ resultats.(i);
		match compare resultats.(List.hd !imaxl) resultats.(i) with
			|0-> imaxl := (i::(!imaxl))
			|(-1)->imaxl:=[i]
			|_-> ()
		done;
	let tmp=Array.of_list !imaxl in let n=Array.length tmp in 
	let choix=tmp.(Random.int n) (*si �galit�, le hasard d�cide [r�gle n�2 ] +  correction issue8 *) in
	(choix, resultats.(choix) > (!sum)/2) (*joueur pl�biscit�, majorit� absolue*)
;;

let array_exists predicat tab=
let i=ref 0 in
while !i<Array.length tab && not (predicat tab.(!i)) do incr i done;
not (!i=Array.length tab)
;;

let array_all predicat tab= not (array_exists (fun x->not (predicat x)) tab);;

