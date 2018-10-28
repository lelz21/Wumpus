(*Lancer Ocaml avec rlwrap ocaml graphics.cma nums.cma*)

open Graphics;;
open Num;;
open List;;

#use "variables.ml";;
#use "fonctions.ml";;

let initGrille() =
  Random.self_init ();

  initTab agent;
  initTab wumpus;

  while (distance agent wumpus) = 0 do
    initTab wumpus;
  done;

  initTab trou;

  if (coin agent) && (distance agent wumpus = 1) then
    begin
      while (distance trou agent) < 2 || (distance trou wumpus) = 0 do
        initTab trou;
      done
    end
  else
    begin
      while (distance trou agent) = 0 || (distance trou wumpus) = 0 do
        initTab trou;
      done;
    end;

  initTab tresor;

  while (distance tresor agent) = 0 || (distance tresor wumpus) = 0 || (distance tresor trou) = 0 do
    initTab tresor;
  done;;

  let printAction str =
    moveto 0 (window_size+text_height/2) ;
    draw_string str;;

(*Fonction qui affiche la grille*)
let drawGrille ()=
  (* On ecrit ce que l'on sent *)
  let (row, col) = lastPos !precedents in
  if isOdeur row col && isBrise row col then
    printAction "Vous sentez une brise ainsi qu'une odeur"
  else
      if isOdeur row col then
        printAction "Vous sentez une odeur"
      else
        if isBrise row col then
          printAction "Vous sentez une brise"
        else
          printAction "Vous traversez la salle sans ne rien percevoir";
  if isTresor row col && !b_arrow
    then begin
      incr arrows;
      b_arrow := false;
      printAction "vous avez trouve une fleche";
    end;

  for i=0 to nbL-1 do
    for j=0 to nbC-1 do
      (* Une case est blanche de base *)
      set_color non_visite;
      (* Si la case a déjà été visitée, on la colorie en de la bonne couleur *)
      if aPrev (i+1) (j+1) !precedents then begin
        set_color prec;
        if isOdeur (i+1) (j+1) then set_color odeur;
        if isBrise (i+1) (j+1) then set_color brise;
        if isBrise (i+1) (j+1) && isOdeur (i+1) (j+1) then set_color both;
      end;
      (* Si le personnage est sur la case courante, on colorie de la bonne couleur *)
      if lastPos !precedents = ((i+1), (j+1)) then
        set_color perso;
      (* On dessine la case *)
      fill_rect (j*window_size/nbC) (i*window_size/nbL) (window_size/nbC) (window_size/nbL);
      (* On dessine les bords de la case *)
      set_color bords;
      draw_rect (j*window_size/nbC) (i*window_size/nbL) (window_size/nbC) (window_size/nbL);
    done;
  done;;

let fin() = lastPos !precedents = (wumpus.(0), wumpus.(1))
         || lastPos !precedents = (trou.(0), trou.(1))
         || !win || !arrows = 0;;

let shoot l c = if !arrows > 0 then isWumpus l c else false;;

(*Le programme*)
let main () =
    (*Initialisation de la fenetre*)
        (* Sous Linux *)
    (* let graph_open = " " ^ string_of_int window_size ^ "x" ^ string_of_int (window_size+text_height) in *)
        (* Sous Windows *)
    let graph_open = " " ^ string_of_int (window_size+15) ^ "x" ^ string_of_int (window_size+50+text_height) in
    open_graph graph_open;
    clear_graph();
    set_window_title "Wumpus";

    (*Dessine dans la fenetre*)
    drawGrille ();
    (*Gestion su clavier*)
    while (not (fin())) do
      drawGrille();
      (* On crée le lecteur d'évenements *)
      let e = wait_next_event [Key_pressed] in
      (* On cherche la case du personnage *)
      let (row, col) = lastPos !precedents in
      (* Lorsqu'une touche est pressée *)
      if e.keypressed then begin
        let key = e.key in
        match key with
        (* Si c'est la touche z, le personnage monte d'une case s'il le peut *)
        | 'w' -> if row < nbL   then precedents := !precedents @ [ (row+1) ; col ]
        (* Si c'est la touche s, le personnage descend d'une case s'il le peut *)
        | 's' -> if row > 1     then precedents := !precedents @ [ (row-1) ; col ]
        (* Si c'est la touche q, le personnage se déplace d'une case à gauche s'il le peut *)
        | 'a' -> if col > 1     then precedents := !precedents @ [ row ; (col-1) ]
        (* Si c'est la touche d, le personnage se déplace d'une case à droite s'il le peut *)
        | 'd' -> if col < nbC   then precedents := !precedents @ [ row ; (col+1) ]
        (**lancement de fleche*)
        | 'W' -> if row < nbL   then if shoot (row+1) col then win := true else decr arrows
        (* Si c'est la touche s, le personnage descend d'une case s'il le peut *)
        | 'S' -> if row > 1     then if shoot (row-1) col then win := true else decr arrows
        (* Si c'est la touche q, le personnage se déplace d'une case à gauche s'il le peut *)
        | 'A' -> if col > 1     then if shoot row (col-1) then win := true else decr arrows
        (* Si c'est la touche d, le personnage se déplace d'une case à droite s'il le peut *)
        | 'D' -> if col < nbC   then if shoot row (col+1) then win := true else decr arrows
        (*default*)
        |  _  -> printAction "";
      end;
      (* On rafraichit l'image *)
      clear_graph();
    done;
    if !win then printAction "Vous avez gagne!"
    else printAction "Vous avez perdu";;

initGrille ();;
main ();;
