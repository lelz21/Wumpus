let wumpus = Array.make 2 0;;
let agent = Array.make 2 0;;
let trou = Array.make 2 0;;
let tresor = Array.make 2 0;;

let tailleMax = 4;;

let alea() = (Random.int tailleMax) + 1;;

let isWumpus l c = wumpus.(0) = l && wumpus.(1) = c;;
let isAgent l c = agent.(0) = l && agent.(1) = c;;
let isTrou l c = trou.(0) = l && trou.(1) = c;;
let isTresor l c = tresor.(0) = l && tresor.(1) = c;;


let coin tab = (tab.(0) = 1 || tab.(0) = tailleMax) && (tab.(1) = 1 || tab.(1) = tailleMax);;

(* distance entre 2 cases *)
let distance tab1 tab2 = abs(tab1.(0) - tab2.(0)) + abs(tab1.(1) - tab2.(1));;

let initTab tab =
tab.(0) <- alea();
tab.(1) <- alea();;

(* 2 éléments ne peuvent pas commencer au même endroit
   si l'agent commence dans un coin, le trou et le wumpus ne
   peuvent pas être placés juste à coté en même temps
*)

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

initGrille();;

print_string (string_of_int(wumpus.(0)) ^ " " ^ string_of_int(wumpus.(1)) ^ "\n");;
print_string (string_of_int(agent.(0)) ^ " " ^ string_of_int(agent.(1)) ^ "\n");;
print_string (string_of_int(trou.(0)) ^ " " ^ string_of_int(trou.(1)) ^ "\n");;
print_string (string_of_int(tresor.(0)) ^ " " ^ string_of_int(tresor.(1)) ^ "\n");;
