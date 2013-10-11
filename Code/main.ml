(* Fonction 1 : génération d'une BlackBox selon la taille de cette dernière et le nombre d'atomes *)

let rec genList = fun
	n -> if (n = 0) then [] else " "::(genList (n-1));;

let rec genListofList = fun
	x y -> if (x = 0) then [] else (genList y)::(genListofList (x-1) y);;

let genBlackBox = fun 
	(x, y) atomes ->
		if (x < 3) || (y < 3) || (atomes > ((x*y) * 1/(min x y))) then
			print_string("L'une des valeur est trop petite. Recommencez !")
		else print_string("Entrée");;