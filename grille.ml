(*Lancer Ocaml avec rlwrap ocaml graphics.cma nums.cma*)

open Graphics;;
open Num;;
open List;;


(*Le tableau des précédents moves*)
let precedents  = ref [2;2];;

(*La taille de la fenetre, ici, la variable est globale*)
let window_size = 400;;


(* Nombre de lignes et colonnes du jeu *)
let nbL = 5;;
let nbC = 5;;

(* Fonction qui vérifie si la case (l,c) a déjà été visitée *)
let rec aPrev l c prec =
  match prec with
    |[] -> false
    |t :: [] -> exit 0
    |t1 :: t2 :: q -> if t1=l && t2=c then true else aPrev l c q;;

(*Fonction pour trouver la position du personnage*)
let rec lastPos prec =
  match prec with
    |[] -> exit 0
    |t :: [] -> exit 0
    |t :: t1 :: [] -> (t, t1)
    |t :: t1 :: q -> lastPos q;;

(*Fonction qui affiche la grille*)
let drawGrille ()=
  for i=0 to nbL-1 do
    for j=0 to nbC-1 do
      (* Une case est blanche de base *)
      set_color white;
      (* Si la case a déjà été visitée, on la colorie en rouge *)
      if aPrev (j+1) (i+1) !precedents then
        set_color red;
      (* Si le personnage est sur la case courante, on colorie en orange *)
      if lastPos !precedents = ((j+1), (i+1)) then
        set_color (rgb 255 155 0);
      (* On dessine la case *)
      fill_rect (i*window_size/nbL) (j*window_size/nbC) (window_size/nbL) (window_size/nbC);
      (* On dessine les bords de la case en noir *)
      set_color black;
      draw_rect (i*window_size/nbL) (j*window_size/nbC) (window_size/nbL) (window_size/nbC);
    done;
  done;;

(*Le programme*)
let main () =
    (*Initialisation de la fenetre*)
        (* Sous Linux *)
    let graph_open = " " ^ string_of_int window_size ^ "x" ^ string_of_int window_size in
        (* Sous Windows *)
        (* let graph_open = " " ^ string_of_int (window_size+20) ^ "x" ^ string_of_int (window_size+50) in *)
    open_graph graph_open;
    clear_graph();
    set_window_title "Wumpus";

    (*Dessine dans la fenetre*)
    drawGrille ();
    (*Gestion su clavier*)
    while true do
      (* On crée le lecteur d'évenements *)
      let e = wait_next_event [Key_pressed] in
      (* On cherche la case du personnage *)
      let (row, col) = lastPos !precedents in
      (* Lorsqu'une touche est pressée *)
      if e.keypressed then begin
        let key = e.key in
        match key with
        (* Si c'est la touche z, le personnage monte d'une case s'il le peut *)
        | 'z' -> if row < nbL   then precedents := !precedents @ [ (row+1) ; col ]
        (* Si c'est la touche s, le personnage descend d'une case s'il le peut *)
        | 's' -> if row > 1     then precedents := !precedents @ [ (row-1) ; col ]
        (* Si c'est la touche q, le personnage se déplace d'une case à gauche s'il le peut *)
        | 'q' -> if col > 1     then precedents := !precedents @ [ row ; (col-1) ]
        (* Si c'est la touche d, le personnage se déplace d'une case à droite s'il le peut *)
        | 'd' -> if col < nbC   then precedents := !precedents @ [ row ; (col+1) ]

      end;
      (* On rafraichit l'image *)
      clear_graph();
      drawGrille();
    done;
    close_graph ();;


main ();;
