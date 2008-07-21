(*il sagit dun .ml juste pour la coloration syntaxique, version a jour, syntaxée en format wiki*)

#summary Résumé de la syntaxe des échanges d'information entre conteur et joueurs

== Introduction ==

Vous trouverez une version plus à jour dans la source dans le ficher "syntaxe des communications (nest pas un ml) .ml"

= Details =

Les informations se font sous la forme dun couple (num de linfo,info sous la forme dun tableau)

_attention_: le tableau ne doit contenir que des entiers (le type information est defini)

||id info||syntaxe||signifie que: ||commentaire||
||idi(-1) ||(-1,...) ||erreur ! ||(peut provenir de la reponse à une question inexistante)||
||idi0 ||(0,[|nbjoueurs:int;id:int|])|| la partie réunit %nbjoueurs%, le joueur qui recoit cette info sera désigné par le numero %id%||depracated:les valeurs en question sont initialisées à la création de lobjet||
||idi1 ||(1,[|untel:int;personnalité:int|]) || %untel% est de telle %personnalité% || (NB utiliser la fonction int2perso)||
||idi2 ||(2,[|untel:int|]) ||je vote pour eliminer %untel%|| ceci est uniquement une réponse de la part des joueurs||
||idi3 ||(3,[|untel:int;cause_de_la_mort:int|]) ||%untel% est mort de la facon %cause_de_la_mort%||la liste des causes de mortalité est dispo plus bas||

les questions seront sous la forme dun couple egalement: (numero de la question, information eventuelement necessaire sous la forme dun tableau)


  * les questions portent des numeros qui essayent de correspondre à une info du meme genre donnée par le conteur
  * la réponse doit se faire sous la forme dun type information (num de l'info, info sous la forme dun tableau)

|| id de la question||syntaxe||signifie:||réponse appropriée:||commentaire||
||idq0 ||(0,[| |])|| "donne moi nb joueur & ton numero"|| idi0||
||idq1 ||(1,[|untel:int|])|| "donne moi lidentité de untel selon toi"|| idi1||
||idq2 ||(2,[|tour:int|]) || "on est au %tour%eme tour, qui veux-tu tuer? "|| idi2|| question (à tous les joueurs) posée le jour _pour lexecution_||


----

==Liste des causes de la mort (identifiants pour idi3)==

||id de la mort||Cause de la mort||
||1||Tué par les loups garous||
||2||Pendu par les villageois||
||..||to be continued||



----

==syntaxe des noms de variables ==

(en theorie il ny a pas de conflit, cest juste pour la comprehension)

  * c_nom : géré par le conteur

----

==fonction qui peuvent etre utilisées==

  * int2perso: int->perso   : associe à un entier la personnalité correspondante
  * perso2int: perso->int : la fonction reciproque (et oui la bijection !)

----

==Règles du jeu implémentées==

  # Deux tours de vote (majorité absolue au 1er tour, relative au second).
  # En cas d'égalité lors d'un vote, le hasard décide.