
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
      assert(x <= y);
      if x <= a && y >= a then true
      else pom t a in
  pom w a
    
let min_wartosc w =
  let rec pom w mini =
    match w with
    | [] -> mini
    | h::t -> let Przedzial(x, y) = h in
      assert(x <= y);
      pom t (min mini x) in
  pom w infinity

let max_wartosc w =
  let rec pom w maks =
    match w with
    | [] -> maks
    | h::t -> let Przedzial(x, y) = h in
      assert(x <= y);
      pom t (max maks y) in
  pom w neg_infinity

let sr_wartosc w =
  match w with
  | h::[] -> let Przedzial(x, y) = h in
    assert(x <= y);
    if x = neg_infinity || y = infinity then nan
    else (x +. y) /. 2.
  | h::t -> nan

let dodawanie (a:przedzial) (b:przedzial) = 
  let Przedzial(ax,ay) = a in
  let Przedzial(bx,by) = b in
  Przedzial(ax+.bx, ay+.by)

let rec pom2 f (p:przedzial) (b2:wartosc) (akk:wartosc) =
      match b2 with
      | [] -> akk
      | h2::t2 -> pom2 f p t2 ( (f p h2)::akk : wartosc )

let dzialanie (f:przedzial->przedzial->przedzial) (a:wartosc) (b:wartosc) =              
  let rec pom1 (a1:wartosc) (b1:wartosc) (ak:wartosc) =
    match a1 with
    | [] -> ak
    | h1::t1 -> 
      pom1 t1 b1  (pom2 f h1 b1 [])@ak  in 
  (pom1 a b []:wartosc);;

let a = [ Przedzial(-10.0, -9.0) ; Przedzial(9.0,10.0)];;
let b = [ Przedzial(-2.0, -1.0); Przedzial(2.0,3.0)];;

let ans = dzialanie dodawanie a b;;
