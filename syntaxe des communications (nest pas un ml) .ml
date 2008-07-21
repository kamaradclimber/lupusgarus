(*il sagit dun .ml juste pour la coloration syntaxique, version a jour, syntax�e en format wiki*)

#summary R�sum� de la syntaxe des �changes d'information entre conteur et joueurs

== Introduction ==

Vous trouverez une version plus � jour dans la source dans le ficher "syntaxe des communications (nest pas un ml) .ml"

= Details =

Les informations se font sous la forme dun couple (num de linfo,info sous la forme dun tableau)

_attention_: le tableau ne doit contenir que des entiers (le type information est defini)

||id info||syntaxe||signifie que: ||commentaire||
||idi(-1) ||(-1,...) ||erreur ! ||(peut provenir de la reponse � une question inexistante)||
||idi0 ||(0,[|nbjoueurs:int;id:int|])|| la partie r�unit %nbjoueurs%, le joueur qui recoit cette info sera d�sign� par le numero %id%||depracated:les valeurs en question sont initialis�es � la cr�ation de lobjet||
||idi1 ||(1,[|untel:int;personnalit�:int|]) || %untel% est de telle %personnalit�% || (NB utiliser la fonction int2perso)||
||idi2 ||(2,[|untel:int|]) ||je vote pour eliminer %untel%|| ceci est uniquement une r�ponse de la part des joueurs||
||idi3 ||(3,[|untel:int;cause_de_la_mort:int|]) ||%untel% est mort de la facon %cause_de_la_mort%||la liste des causes de mortalit� est dispo plus bas||

les questions seront sous la forme dun couple egalement: (numero de la question, information eventuelement necessaire sous la forme dun tableau)


  * les questions portent des numeros qui essayent de correspondre � une info du meme genre donn�e par le conteur
  * la r�ponse doit se faire sous la forme dun type information (num de l'info, info sous la forme dun tableau)

|| id de la question||syntaxe||signifie:||r�ponse appropri�e:||commentaire||
||idq0 ||(0,[| |])|| "donne moi nb joueur & ton numero"|| idi0||
||idq1 ||(1,[|untel:int|])|| "donne moi lidentit� de untel selon toi"|| idi1||
||idq2 ||(2,[|tour:int|]) || "on est au %tour%eme tour, qui veux-tu tuer? "|| idi2|| question (� tous les joueurs) pos�e le jour _pour lexecution_||


----

==Liste des causes de la mort (identifiants pour idi3)==

||id de la mort||Cause de la mort||
||1||Tu� par les loups garous||
||2||Pendu par les villageois||
||..||to be continued||



----

==syntaxe des noms de variables ==

(en theorie il ny a pas de conflit, cest juste pour la comprehension)

  * c_nom : g�r� par le conteur

----

==fonction qui peuvent etre utilis�es==

  * int2perso: int->perso   : associe � un entier la personnalit� correspondante
  * perso2int: perso->int : la fonction reciproque (et oui la bijection !)

----

==R�gles du jeu impl�ment�es==

  # Deux tours de vote (majorit� absolue au 1er tour, relative au second).
  # En cas d'�galit� lors d'un vote, le hasard d�cide.