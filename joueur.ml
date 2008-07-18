let nbjoueurs=ref 0;;
let whoswho=ref [||];;
let mon_num=ref 0;;
open Definition

let rec info (id_info,contenu)=
(*pkoi rec ? -> si jamais une information en génère une autre*)
match id_info with
|0 -> (*initialisation*) 
	begin
	nbjoueurs:= contenu.(0);
	whoswho:=Array.make !nbjoueurs (None : perso option)
;
	mon_num := contenu.(1)
	end
|_-> ();;
