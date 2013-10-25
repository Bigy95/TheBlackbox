(* Fonction 1 : génération d'une BlackBox selon la taille de cette dernière et le nombre d'atomes *)
	exception ValeursInvalides;;
	exception ListeVide;;
	exception SuperpositionAtomes;;

(* Initialisation des modules *)
#load "unix.cma";;

(* Initialisation de Random *)
	let _ = Random.self_init();;

(* Genération d'une grille vide *)

	let rec genList = fun
		n -> if (n = 0) then [] else " "::(genList (n-1));;

	let rec genListofList = fun
		x y -> if (x = 0) then [] else (genList y)::(genListofList (x-1) y);;

(* Generation aléatoire d'un couple de valeurs *)
	let rnd = fun
		a ->  let x = truncate ((mod_float (Unix.gettimeofday()) 1.) /. 0.000001) in
			  let y = truncate ((mod_float (Unix.gettimeofday()) 1.) /. 0.000001) in  
			(x*x + ((y*y)-x)) mod a;;

	let rnd_atom = fun
		(a, b) -> ((rnd (b+1-a))+a, (rnd (b+1-a))+a);;

(* Placement des atomes *)

	let rec copy = fun
		liste -> if (liste = [])
					then []
				else
					(List.hd liste)::(copy (List.tl liste));;

	let rec modif = fun
		liste x i -> if (liste = [])
						then []
					 else (
						if (i = x) then
							if (List.hd liste = "O")
								then raise SuperpositionAtomes
							else "O"::(modif (List.tl liste) x (i+1))
						else
							(List.hd liste)::(modif (List.tl liste) x (i+1) )
						);;

	let rec addAtomRec = fun
		liste (x, y) i -> if(liste = [])
								then [] 
							else (
								if (i = y) then
									let premier = (modif (List.hd liste) x 1)
									in let reste = (addAtomRec (List.tl liste) (x, y) (i+1)) 
									in premier::reste
								else
									let premier = (copy (List.hd liste))
									in let reste = (addAtomRec (List.tl liste) (x, y) (i+1))
									in premier::reste
								);;

	let rec addAtom = fun
		liste (x, y) -> if (liste = [])
							then raise ListeVide
						else 
							try (addAtomRec liste (x, y) 1) with
								 | SuperpositionAtomes -> print_string("ENTRY");(addAtom liste (rnd_atom (1,y)));;

	let rec genAtoms = fun
			plateau nbAtoms (x, y) -> if (nbAtoms < 1) then
							plateau
						else 
							let plateau2 = addAtom plateau (rnd_atom (1,y)) in
							genAtoms plateau2 (nbAtoms-1) (x, y) ;
						;;

(* Affichage du plateau en ASCII *)

	(* [Required : This function adds the ability to convert a char into a string] *)
	let string_of_char ch = String.make 1 ch;;
	(* [/Required] *)

	let rec printLigneRec = fun 
		i -> if (i < 1)
				then "-"
			else
				"----" ^ printLigneRec (i-1);;

	let printLigne = fun
		liste -> " " ^ printLigneRec (List.length liste) ^ "\n";;

	let rec affichageLigne = fun 
		colonne -> if (colonne = [])
						then " |"
					else " | "^(List.hd colonne)^(affichageLigne (List.tl colonne));;

	let rec affichagePlateauRec = fun 
		liste -> if (liste = [])
						then ""
					else (
							affichageLigne (List.hd liste) ^ "\n" ^ (printLigne (List.hd liste)) ^ (affichagePlateauRec (List.tl liste))
						);;

	let showPlateau = fun
		plateau -> if (plateau = [])
						then print_string("Maps is empty ! Please create one first")
					else 
						print_string((printLigne (List.hd plateau)) ^ affichagePlateauRec plateau)
					;;

(* Récupération de la valeurs d'une case *)

	let rec getValueRec = fun
		(x, y) liste j -> if (j = y) then (List.hd liste) else getValueRec (x, y) (List.tl liste) (j+1);;

	let rec recupValueRec = fun
		(x, y) plateau i -> if (i = x) then getValueRec (x, y) (List.hd plateau) 1 else recupValueRec (x, y) (List.tl plateau) (i+1);;

	let recupValue = fun
		(x, y) plateau -> if plateau = [] then "" else recupValueRec (x, y) plateau 1;;

(* Génération de la grille complète *)

	let genBlackBox = fun 
		(x, y) atomes ->
			if (x < 3) || (y < 3) || (atomes > (2*(x*y)/(min x y))) then
				raise ValeursInvalides
			else (
				(genListofList x y);
			);;


let plateau = genBlackBox (4, 4) 4;;
let plateau = genAtoms plateau 4 (4,4);;
showPlateau (genAtoms (genBlackBox (4, 4) 4) 4 (4,4));;