
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

let alea() = (Random.int tailleMax) + 1;;

let isWumpus l c = wumpus.(0) = l && wumpus.(1) = c;;
let isAgent l c = agent.(0) = l && agent.(1) = c;;
let isTrou l c = trou.(0) = l && trou.(1) = c;;
let isTresor l c = tresor.(0) = l && tresor.(1) = c;;

let isBrise l c =  isTrou (l-1) (c-1) || isTrou (l-1) c || isTrou (l-1) (c+1)
                || isTrou l (c-1)     || isTrou l (c+1)
                || isTrou (l+1) (c-1) || isTrou (l+1) c || isTrou (l+1) (c+1);;

let isOdeur l c =  isWumpus (l-1) (c-1) || isWumpus (l-1) c || isWumpus (l-1) (c+1)
                || isWumpus l (c-1)     || isWumpus l (c+1)
                || isWumpus (l+1) (c-1) || isWumpus (l+1) c || isWumpus (l+1) (c+1);;

let isChoc l c = l <= 0 || l > nbL || c <= 0 || c > nbC;;



let coin tab = (tab.(0) = 1 || tab.(0) = tailleMax) && (tab.(1) = 1 || tab.(1) = tailleMax);;

(* distance entre 2 cases *)
let distance tab1 tab2 = abs(tab1.(0) - tab2.(0)) + abs(tab1.(1) - tab2.(1));;

let initTab tab =
tab.(0) <- alea();
tab.(1) <- alea();;

let fin() = lastPos !precedents = (wumpus.(0), wumpus.(1))
         || lastPos !precedents = (trou.(0), trou.(1))
         || !win || !loose || !arrows < 0 ||
         (!arrows = 0 && !b_arrow = false);;

let shoot l c = if !arrows > 0 then isWumpus l c else false;;


let printArrows()=
  moveto (window_size - 100) (window_size+text_height/2);
  let str = "fleches : " ^ string_of_int !arrows in
  draw_string str;;

let printAction str1 str2 =
  printArrows();
  moveto 0 (window_size+text_height/2);
  draw_string str1;
  moveto 0 (window_size+text_height/2-text_height/4);
  draw_string str2;;

let reinitialize() =
  precedents := [];
  win := false;
  loose := false;
  arrows := 1;
  b_arrow := true;;
