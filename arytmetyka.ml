
type przedzial = Przedzial of float * float | Pelny | Pusty

type wartosc = przedzial list
  
let wartosc_dokladnosc x p =
  let delta = abs_float(p /. 100. *. x) in
  (Przedzial(x -. delta , x +. delta)::[]:wartosc)

let wartosc_od_do x y = (Przedzial(x,y)::[]:wartosc)

let wartosc_dokladna x = (Przedzial(x,x)::[]:wartosc)

let in_wartosc (w:wartosc) (a:float) =
  let rec pom (w:wartosc) (a:float) =
    match w with
    | [] -> false
    | h::t -> let Przedzial(x, y) = h in
      if x <= a && y >= a then true
      else pom t a
    
let min_wartosc w =
  let rec pom w mini =
    match w with
    | [] -> mini
    | h::t -> let Przedzial(x, y) = h in
      pom t (min mini x)

let max_wartosc w =
  let rec pom w maks =
    match w with
    | [] -> maks
    | h::t -> let Przedzial(x, y) = h in
      pom t (max maks x)

let sr_wartosc w =
  match w with
  | h::[] -> let Przedzial(x, y) = h in
    if x = neg_infinity || y = infinity then nan
    else (x +. y) /. 2.
  | h::t -> nan
    
 
let () = assert(1 <= 2)



