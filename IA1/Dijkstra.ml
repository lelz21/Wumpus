let voisin = ref [];;
let p = ref [];;
let cout s =
  s * 99 + 1 ;;
(* Le voisin en haut *)
let voisinH matrice (x, y) =
  if x != nbL-1 then (x+1, y)
  else (-1, -1);;
(* Le voisin en bas *)
let voisinB matrice (x, y) =
  if x != 0 then (x-1, y)
  else (-1, -1);;
(* Le voisin à droite *)
let voisinD matrice (x, y) =
  if y != nbL-1 then (x, y+1)
  else (-1, -1);;
(* Le voisin à gauche *)
let voisinG matrice (x, y) =
  if y != 0 then (x, y-1)
  else (-1, -1);;
let rec voisins matrice (x, y) notin =
  voisin := [];
  let (x0, y0) = voisinH matrice (x, y) in
  if  x0 != -1 && y0 != -1 && (isIn notin (x0, y0))=false then voisin := !voisin @ [x0 ; y0];
  let (x1, y1) = voisinB matrice (x, y) in
  if  x1 != -1 && y1 != -1 && (isIn notin (x1, y1))=false then voisin := !voisin @ [x1 ; y1];
  let (x2, y2) = voisinG matrice (x, y) in
  if  x2 != -1 && y2 != -1 && (isIn notin (x2, y2))=false then voisin := !voisin @ [x2 ; y2];
  let (x3, y3) = voisinD matrice (x, y) in
  if  x3 != -1 && y3 != -1 && (isIn notin (x3, y3))=false then voisin := !voisin @ [x3 ; y3];;
let rec isIn liste (x, y) =
  match liste with
  | [] -> false
  | t :: [] -> exit 0
  | t1 :: t2 :: q -> if t1=x && t2=y then true else isIn q (x, y);;
let rec getAt liste ind =
  match liste with
  | t1 :: t2 :: q -> if ind = 0 then (t1, t2) else getAt q (ind-1)
  | [] -> (-1, -1);;
let rec half_length liste =
  match liste with
  |[e] -> exit 0
  |[] -> 0
  |t :: t1 :: q -> 1 + half_length q;;
let rec printListe liste =
  match liste with
  | [] -> print_string "__"
  | [e] -> print_int e
  | t :: q -> print_int t; print_string " " ; printListe q;;
let dijkstra matrice (x1, y1) (x2, y2) =
  p := [x1; y1];
  let d = Array.make_matrix nbL nbC 100000000 in
  d.(x1).(y1) <- 0;
  let x = ref 0 in
  let y = ref 0 in
  let cost  = ref 100000000 in
  let xActu = ref x1 in
  let yActu = ref y1 in
  let minVal = ref 100000000 in
  let indice = Array.make 2 0 in
  while isIn !p (x2, y2) = false do
    voisins matrice (!xActu, !yActu) !p;

    minVal := 100000000;
    for i = 0 to ((half_length !voisin) -1) do
      let (row, col) = getAt !voisin i in
      x := row;
      y := col;
      cost := cout matrice.(!x).(!y);
      if d.(!x).(!y) > !cost then
        d.(!x).(!y) <- !cost;
      if !minVal > d.(!x).(!y) then begin
        minVal := d.(!x).(!y);
        indice.(0) <- !x;
        indice.(1) <- !y;
      end;
      if !x = x2 && !y = y2 then begin
        minVal := -10;
        indice.(0) <- !x;
        indice.(1) <- !y;
      end
    done;
    p := !p @ [ indice.(0); indice.(1) ];
    xActu := indice.(0);
    yActu := indice.(1);
  done;;
