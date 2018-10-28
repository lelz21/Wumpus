(*Le tableau des précédents moves*)
let precedents  = ref [2;2];;
let win = ref false;;
let arrows = ref 1;;
let b_arrow = ref true;;

(*La taille de la fenetre, text_height sera ajouté à la hauteur*)
let window_size = 400;;

(* La hauteur de l'espace pour décrire les actions *)
let text_height = 50;;

(* Couleurs *)
let odeur = green;;
let brise = blue;;
let both = rgb 0 255 255;;
let perso = red ;;
let prec = rgb 255 155 0;;
let non_visite = white;;
let bords = black;;

(* Nombre de lignes et colonnes du jeu *)
let nbL = 4;;
let nbC = 5;;

let wumpus = Array.make 2 0;;
let agent = Array.make 2 0;;
let trou = Array.make 2 0;;
let tresor = Array.make 2 0;;

let tailleMax = 4;;
