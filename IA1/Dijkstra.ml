let nbL = 4;;
let nbC = 4;;
let tabFinal = Array.make_matrix nbL nbC 0;;

let printTabs() =
  for i = 0 to nbL-1 do
    for j = 0 to nbC-1 do
      print_int tabFinal.(i).(j);
    done;
    print_endline "";
  done;;


tabFinal.(0).(0) <- 1;;
tabFinal.(0).(1) <- 1;;
tabFinal.(1).(1) <- 1;;
tabFinal.(1).(2) <- 1;;
tabFinal.(2).(2) <- 1;;

(* Le voisin en haut *)
let voisinH matrix (x, y) =
  if x != nbL-1 then (x-1, y)
  else (-1, -1);;
(* Le voisin en bas *)
let voisinB matrix (x, y) =
  if y != 0 then (x, y-1)
  else (-1, -1);;
(* Le voisin à droite *)
let voisinD matrix (x, y) =
  if x != nbL-1 then (x+1, y)
  else (-1, -1);;
(* Le voisin à gauche *)
let voisinG matrix (x, y) =
  if x != 0 then (x-1, y)
  else (-1, -1);;


let dijkstra matrix (x1, y1) (x2, y2) =
  let (row, col) = voisinB matrix (1, 1) in
  print_int row;
  print_int col;
;;




printTabs();;
dijkstra tabFinal (0,0) (2,2);;

print_endline "";
