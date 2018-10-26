
type przedzial = Przedzial of float * float

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

let mnozenie (a:przedzial) (b:przedzial) =
  let Przedzial(w,x) = a in
  let Przedzial(y,z) = b in
  let nx = min (min (w*.y) (w*.z)) (min (x*.y) (x*.z)) in
  let ny = max (max (w*.y) (w*.z)) (max (x*.y) (x*.z)) in
  Przedzial(nx, ny)

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

(************************************************************)
let a = [ Przedzial(-10.0, -9.0) ; Przedzial(9.0,10.0)];;
let b = [ Przedzial(-2.0, -1.0); Przedzial(2.0,3.0)];;

let ans = dzialanie dodawanie a b;;
(************************************************************)

let c = [ Przedzial(neg_infinity,-10.0); Przedzial(2.0, infinity);
          Przedzial(neg_infinity,-12.0); Przedzial(10.0, infinity)]

let kompresuj (w:wartosc) =
  match w with
  | [] -> w
  | h::[] -> w
  | h::t -> let rec pom l mini maks =
     match l with
     | [] -> if mini >= maks then [Przedzial(neg_infinity,infinity)]
       else [ Przedzial( neg_infinity, mini ); Przedzial( maks, infinity ) ]
     | Przedzial(x,y)::t -> if x = neg_infinity then pom t (max mini y) maks
       else pom t mini (min maks x) in
    pom w neg_infinity infinity
      
let plus (a:wartosc) (b:wartosc) =
  kompresuj (dzialanie dodawanie a b)

let razy (a:wartosc) (b:wartosc) =
  kompresuj (dzialanie mnozenie a b)

let przeciwny (a:wartosc) =
  let rec pom (a:wartosc) (ak:wartosc) =
    match a with
    | [] -> ak
    | Przedzial(x,y)::t -> pom t (Przedzial(-.y,-.x)::ak) in
  pom a []

let minus (a:wartosc) (b:wartosc) =
  plus a (przeciwny b)

let odwrotny (a:wartosc) =
  match a with
  | Przedzial(neg_infinity,a)::Przedzial(b,infinity)::_ ->
    assert(a < b);
    if a = 0. then Przedzial(neg_infinity, 1. /. b)::[]
    else if b = 0. then Przedzial(1. /. a, infinity)::[]
    else if a *. b < 0. then Przedzial(1. /. a, 1. /. b)::[]
    else Przedzial(1. /. b, 1. /. a)::[]
  | Przedzial(a,b)::[] ->
    let ia = 1. /. a in
    let ib = 1. /. b in
    assert(a <= b);
    if a = neg_infinity && b = infinity then Przedzial(neg_infinity, infinity)::[]
    else if a = 0. && b = 0. then []
    else if a = 0. then Przedzial(ib, infinity)::[]
    else if b = 0. then Przedzial(neg_infinity, ia)::[]
    else if a *. b < 0. then Przedzial(neg_infinity, ia)::Przedzial(ib,infinity)::[]
    else Przedzial(neg_infinity, ib)::Przedzial(ia, infinity)::[]

let podzielic (a:wartosc) (b:wartosc) =
  razy a (odwrotny b)
        
