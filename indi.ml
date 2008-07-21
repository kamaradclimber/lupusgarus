(*les fonctions sans rapport mais indispensables*)
Random.init (int_of_float (Unix.time ()));;


let pause ()=
	Sys.catch_break true;
	Printf.printf "\n%s\n" "on fait la pause, ctrl+c pour reprendre";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend"
;;

let print_int_tab tab= Array.iter (fun x->Printf.printf "%i " x) tab;print_newline ();;

let vote_majorite (resultats:int array)=
	let imax=ref 0 in
	for i=1 to Array.length resultats-1 do (*le fait de commencer à 1 causera un bug s'il n'y a qun joueur*)
		match compare resultats.(!imax) resultats.(i) with
			|0->(*égalité, le hasard décide [règle n°2 ]*) if Random.bool () then imax:=i
			|1->imax:=i
			|_-> ()
		done;
	(!imax,!imax>(Array.length resultats)/2+1) (*joueur plébiscité, majorité absolue*)
;;
