(*il sagit dun .ml juste pour la coloration syntaxique*)

les informations se font sous la forme dun couple (type de linfo,info sous la forme dun tableau)
attention: le tableau ne doit contenir que des entiers

type0 (0,[|nbjoueurs:int;id:int|]) signifie que la partie réunit %nbjoueurs%, le joueur qui recoit cette info sera désigné par le numero %id%
type1 (1,[|untel:int;personnalité:int|]) signifie %untel% est de telle %personnalité% (NB utiliser la fonction num2perso)

les questions seront sous la forme dun couple egalement: (numero de la question* information eventuelement necessaire sous la forme dun tableau)




(---------------------------)
syntaxe des noms de variables (en theorie il ny a pas de conflit, cest juste pour la comprehension)

c_nom : géré par le conteur

(---------------------------)
fonction qui peuvent etre utilisées
num2perso: int->perso   : associe à un entier la perosnnalité correspondante
perso2num: perso->int : la fonction reciproque (et oui la bijection !)
