let pause ()=
	Sys.catch_break true;
	Printf.printf "%s\n" "on fait la pause, ctrl+c pour arreter";
	flush stdout;
	let rec pause2 ()=pause2 () in
	try  pause2 () with Sys.Break -> Printf.printf "%s\n" "on reprend";flush stdout
;;



	
	
	
	
	
pause ();;

