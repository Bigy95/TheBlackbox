Let lanceray = fun (x,y) -> match (w,y) with
	(0,0) -> print_string("Rayon invalide !")
	|(0,n) -> print_string("Rayon invalide !")
	|(m,0) -> print_string("Rayon invalide !")
	|(m,n) -> print_string("Rayon invalide !")
	|(_,0) -> rayon vers le bas
	|(0,_) -> rayon vers la droite
	|(_,n) -> rayon vers le haut
	|(m,_) -> rayon vers le haut
	|(_,_) -> print_string("Rayon invalide !") ;;
	
