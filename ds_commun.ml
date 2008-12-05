let nbjoueurs=(Random.int 17) + 5;;
let id=Random.int nbjoueurs;;
let nb_personnalites = 2;;
let nb_causes_morts = 2;;
let taupin=((new Joueur.joueur_de_base nbjoueurs id) :> Definition.joueur);;



print_string "Tests de communication:...\n";;
print_string "Si des avertissements apparaissent, c'est bien; sinon c'est mal !\n";;
let erreurs_info=ref 4;;


print_string "idi1....";;
try
for i=0 to 100 do
taupin#donne_info (1,[|Random.int nbjoueurs;Random.int nb_personnalites|])
done;
decr erreurs_info;
print_string "ok\n"
with _->print_string "erreur lors du traitement de l'information\n"
;;

print_string "idi2....na pas besoin detre testée\n";;

print_string "idi3....";;
try
for i=0 to 100 do
taupin#donne_info (3,[|Random.int nbjoueurs;Random.int nb_causes_morts|])
done;
decr erreurs_info;
print_string "ok\n"
with _->print_string "erreur lors du traitement de l'information\n"
;;

print_string "idi4....";;
try
for i=0 to 100 do
taupin#donne_info (4,[|0;Random.int 2;Random.int 2;Random.int nbjoueurs;Random.int nbjoueurs |])
done;
decr erreurs_info;
print_string "ok\n"
with _->print_string "erreur lors du traitement de l'information\n"
;;


print_string "idi5....na pas besoin detre testée\n";;

print_string "idi6....";;
try
taupin#donne_info (6,[|0|]);
decr erreurs_info;
print_string "ok\n"
with _->print_string "erreur lors du traitement de l'information\n"
;;



let erreurs_questions=ref 6;;

print_string "idq0....";;
try
let (idi,reponse)=taupin#pose_question (0,[||]) in
if idi<>0  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>2 then print_string "echec: mauvais format de réponse\n"
else if reponse.(0)<>nbjoueurs then print_string "echec: mauvais nb de joueurs\n"
else if reponse.(1)<>id then print_string "echec: mauvais id\n"
else (decr erreurs_questions ;print_string "ok\n") with _->print_string "erreur lors du traitement de la question\n"
;;

print_string "idq1....";;
try
let untel=Random.int nbjoueurs in
let (idi,reponse)=taupin#pose_question (1,[|untel|]) in
if idi<>1  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>2 then print_string "echec: mauvais format de reponse\n"
else if reponse.(0)<>untel then print_string "echec: mauvais %untel%\n"
else try (ignore (Definition.int2perso reponse.(1));decr erreurs_questions ; print_string "ok\n") with _-> print_string "echec: personnalité inconnue\n" with _->print_string "erreur lors du traitement de la question\n"
;;

print_string "idq2....";;
try
let (idi,reponse)=taupin#pose_question (2,[||]) in
if idi<>2  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>1 then print_string "echec: mauvais format de reponse\n"
else if reponse.(0)<0 || reponse.(0)>=nbjoueurs then print_string "echec: ce joueur n'existe pas\n"
else (decr erreurs_questions ;print_string "ok\n") with _->print_string "erreur lors du traitement de la question\n"
;;

print_string "idq3....";;
try
taupin#donne_info (1,[|taupin#get_id;Definition.perso2int Definition.Loup|]);
let tour=1 in
let (idi,reponse)=taupin#pose_question (3,[|tour|]) in
if idi<>2  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>1 then print_string "echec: mauvais format de reponse\n"
else if reponse.(0)<0 || reponse.(0)>=nbjoueurs then print_string "echec: ce joueur n'existe pas\n"
else (decr erreurs_questions ;print_string "ok\n") with _ -> print_string "erreur lors du traitement de la question\n"
;;

print_string "idq4....";;
try
taupin#donne_info (1,[|taupin#get_id;Definition.perso2int Definition.Voyante|]);
let (idi,reponse)=taupin#pose_question (4,[||]) in
if idi<>2  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>1 then print_string "echec: mauvais format de reponse\n"
else if reponse.(0)<0 || reponse.(0)>=nbjoueurs then print_string "echec: ce joueur n'existe pas\n"
else (decr erreurs_questions ;print_string "ok\n") with _->print_string "erreur lors du traitement de la question\n"
;;

print_string "idq5....";;
try
taupin#donne_info (1,[|taupin#get_id;Definition.perso2int Definition.Sorciere|]);
let untel=0 in
let (idi,reponse)=taupin#pose_question (5,[|untel|]) in
if idi<>5  then print_string "echec : mauvaise idi\n" 
else if Array.length reponse <>3 then print_string "echec: mauvais format de reponse\n"
else if reponse.(0)<>0 && reponse.(0)<>1  then print_string "echec: mauvais %tuer%, la valeur doit etre 0 ou 1\n"
else if reponse.(1)<0 || reponse.(1)>=nbjoueurs then print_string "echec: ce joueur n'existe pas\n"
else if reponse.(2)<>0 && reponse.(2)<>1  then print_string "echec: mauvais %sauver%, la valeur doit etre 0 ou 1\n"
else (decr erreurs_questions ;print_string "ok\n") with _->print_string "erreur lors du traitement de la question\n"
;;


Printf.printf "Erreurs questions: %i\n\n" !erreurs_questions;;
Printf.printf "Erreurs information: %i\n\n" !erreurs_info;;

print_string "Tests avancés\n";;

print_string "Test n°1...";;
try ignore (new Joueur.joueur_de_base nbjoueurs (-1));ignore (new Joueur.joueur_de_base nbjoueurs nbjoueurs );print_string "echec\n" with _->print_string "ok\n";;



flush stdout;;

if Sys.os_type = "Unix"
then print_float (Sys.time ())
else ignore(Sys.command "pause")
;;
print_string "\n"
;;
