let regle = fun () ->
	 print_string("Il suffit de placer un nombre d'atomes (préalablement choisi par le joueur) dans une grille 
	 de dimension préalablement choisi par le joueur). Ensuite, le joueur adverse doit désigner une case périphérique 
	 à la grille. Sur cette case, nous dirons qu'un émetteur laser vient d'être placé. Si possible, le joueur devra 
	 donc deviner la position d'un, voire de plusieurs atomes en fonction des déviations du faisceau. Comme le nom du 
	 jeu l'indique, le joueur ne connaîtra pas la trajectoire exacte du laser, mais par quelle case périphérique il sortira 
	 de la boîte noire. Le joueur peut placer autant d'émetteurs laser que le permet le plateau, mais plus il en utilise, 
	 et moins il gagnera de points. ");;

let credit = fun () ->
	 print_string("Jeu rÃ©alisÃ© par Famelart Valentin, Bard Alan et Joret Bastien");;

let jouer = fun () ->
	 print_string("Lancement du jeu");;



let menu = fun i -> match i with
	1 -> jouer()
	|2 -> regle()
	|3 -> credit()
	|4 -> ();;
