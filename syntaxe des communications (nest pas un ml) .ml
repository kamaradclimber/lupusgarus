(*il sagit dun .ml juste pour la coloration syntaxique*)

les informations se font sous la forme dun couple (num de linfo,info sous la forme dun tableau)
attention: le tableau ne doit contenir que des entiers (le type information est defini)


idi(-1) (-1,_) erreur ! (peut provenir de la reponse � une question iniexistante)
idi0 (0,[|nbjoueurs:int;id:int|]) signifie que la partie r�unit %nbjoueurs%, le joueur qui recoit cette info sera d�sign� par le numero %id%
idi1 (1,[|untel:int;personnalit�:int|]) signifie %untel% est de telle %personnalit�% (NB utiliser la fonction num2perso)

les questions seront sous la forme dun couple egalement: (numero de la question* information eventuelement necessaire sous la forme dun tableau)
-les question portent des numeros qui essayent de correspondre � une info du meme genre donn�e par le conteur
-la reponse doit se faire sous la forme dun type information (num de l'info, info sous la forme dun tableau)

idq0 (0,[||]) signifie "donne moi nb joueur & ton numero", reponse: idi0
idq1 (1,[|untel:int|]) signifie "donne moi lidentit� de untel selon toi", reponse: idi1




(---------------------------)
syntaxe des noms de variables (en theorie il ny a pas de conflit, cest juste pour la comprehension)

c_nom : g�r� par le conteur

(---------------------------)
fonction qui peuvent etre utilis�es
num2perso: int->perso   : associe � un entier la personnalit� correspondante
perso2num: perso->int : la fonction reciproque (et oui la bijection !)
