let nbL = 4;;
let nbC = 4;;
let tabFinal = Array.make_matrix nbL nbC 0;;
let distance = Array.make_matrix nbL nbC 10000000000;;

let printTabs tab =
  for i = 0 to nbL-1 do
    for j = 0 to nbC-1 do
      print_int tab.(i).(j);
      print_string " ";
    done;
    print_endline "";
  done;;

let arc01 = 100;;
let arc10 = 100;;
let arc11 = 1;;
let arc00 = 1;;

tabFinal.(0).(0) <- 1;;
tabFinal.(0).(1) <- 1;;
tabFinal.(1).(1) <- 1;;
tabFinal.(1).(2) <- 1;;
tabFinal.(2).(2) <- 1;;

(* Le voisin en haut *)
let voisinH matrix (x, y) =
  if x != nbL-1 then (x+1, y)
  else (-1, -1);;
(* Le voisin en bas *)
let voisinB matrix (x, y) =
  if x != 0 then (x-1, y)
  else (-1, -1);;
(* Le voisin à droite *)
let voisinD matrix (x, y) =
  if y != nbL-1 then (x, y+1)
  else (-1, -1);;
(* Le voisin à gauche *)
let voisinG matrix (x, y) =
  if y != 0 then (x, y-1)
  else (-1, -1);;

let rec min lst val =
  match lst with
    | [] -> val
    | t::q -> if t < val then (min q t) else (min q val);;

(* let dijkstra matrix (x1, y1) (x2, y2) =
  ;; *)



printTabs tabFinal;;
print_endline "";
print_endline "";
printTabs distance;;
print_endline "";

(* Donne des coordonnées entre 0 et nbL-1 (à remettre entre 1 et nbL)) *)

(* dijkstra tabFinal (0,0) (2,2);; *)
