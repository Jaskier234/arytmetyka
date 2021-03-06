(* autor: Artur Matyjasek *)
(* code review: Michał Makowski *)

open List
  
(* Reprezentacja przedziałów.
   Typ wartosc to lista przedziałów długości:
   - 0 przedzial pusty   
   - 1 [x,y] gdzie x <= y 
   - 2 [-inf,x]U[y,inf] gdzie x < y 
   Przedział pełny to przedział [-inf,inf]
   *)
type przedzial = Przedzial of float * float
type wartosc = przedzial list


(* konstruktory *)
let wartosc_dokladnosc x p =
  let delta = abs_float (p /. 100. *. x) in
  Przedzial(x -. delta, x +. delta) :: [] 

let wartosc_od_do x y = Przedzial(x, y) :: []

let wartosc_dokladna x = Przedzial(x, x) :: []

let in_wartosc wart a =
  let rec in_przedzial (w:wartosc) (a:float) =
    match w with
    | [] -> false
    | h :: t -> let Przedzial(x, y) = h in
      assert(x <= y);
      if x <= a && y >= a then true
      else in_przedzial t a in
  in_przedzial wart a
    
let min_wartosc wart =
  if wart = [] then nan
  else let rec min_przedzial (w:wartosc) (mini:float) =
    match w with
    | [] -> mini
    | h :: t -> let Przedzial(x, y) = h in
      assert(x <= y);
      min_przedzial t (min mini x) in
  min_przedzial wart infinity

let max_wartosc wart =
  if wart = [] then nan
  else let rec max_przedzial (w:wartosc) (maks:float) = 
    match w with
    | [] -> maks
    | h :: t -> let Przedzial(x, y) = h in
      assert(x <= y);
      max_przedzial t (max maks y) in
  max_przedzial wart neg_infinity

let sr_wartosc wart =
  match wart with
  | h :: [] -> let Przedzial(x, y) = h in
    assert(x <= y);
    (x +. y) /. 2.
  | h :: t -> nan (* wartosc jest postaci [-inf,x]U[y,inf]. Nie ma sredniej *)
  | [] -> nan
    
let dodawanie (a:przedzial) (b:przedzial) =
  (* funkcja pomocnicza, która dodaje 2 przedziały *)
  let Przedzial(ax, ay) = a in
  let Przedzial(bx, by) = b in
  Przedzial(ax +. bx, ay +. by)



let mnozenie (a:przedzial) (b:przedzial) =
  (* funkcja pomocnicza, która mnoży 2 przedziały *)
  let Przedzial(w, x) = a in
  let Przedzial(y, z) = b in
  let ogr = [w *. y; w *. z; x *. y; x *. z] in

  let not_nan a = if classify_float a = FP_nan then false else true in
  let mini l = filter not_nan l |> fold_left min infinity in
  let maks l = filter not_nan l |> fold_left max neg_infinity in

  let is_sc1 wart =
    let Przedzial(xw, yw) = wart in 
    if xw = neg_infinity || yw = infinity then true else false in
  let is_sc2 wart =
    let Przedzial(xw, yw) = wart in
    if xw = 0. || yw = 0. then true else false in

  (* specjalny przypadek dla przedziałów którtch końcami są -inf, inf lub 0
  wtedy należy rozszerzyć listę ogr *)
  if (is_sc1 a && is_sc2 b) ||(is_sc1 b && is_sc2 a) then
    let sign wart =
      let Przedzial(xw, yw) = wart in
      if xw = 0. && yw = 0. then 0.
      else if xw = neg_infinity || yw = 0. then -1.
      else 1. in
    let lista2 = ogr @ ((infinity *. sign a *. sign b) :: 0. :: []) in
    Przedzial(mini lista2, maks lista2)  
  else Przedzial(mini ogr, maks ogr)


let dzialanie (f:przedzial->przedzial->przedzial) (a:wartosc) (b:wartosc) =
  (* funkcja przyjmująca 2 argumenty typu wartosc i zwracająca typ wartosc.
  Działa funkcją 'f'(dodawanie/mnozenie) na przedziały z a i b (każdy z każdym)
  i zwraca listę tych przedziałów. Wyjściowa lista nie musi być poprawnym 
  elementem typu wartosc (może mieć do 4 przedziałów) *)  
  let rec pom1 (a1:wartosc) (b1:wartosc) (ak:wartosc) =
    match a1 with
    | [] -> ak
    | h1 :: t1 -> let rec pom2 (p:przedzial) (b2:wartosc) (akk:wartosc) =
      match b2 with
      | [] -> akk
      | h2 :: t2 -> pom2 p t2 ((f p h2) :: akk:wartosc) in 
      pom1 t1 b1  (pom2 h1 b1 [])@ak in 
  (pom1 a b []:wartosc);;

let kompresuj (w:wartosc) =
  (* Przyjmuje typ wartosc dowolnej długości i zwraca typ wartosc spełniący 
  początkowe założenia *)
  match w with
  | [] -> w
  | h :: [] -> w
  | h :: t -> let rec pom l mini maks =
    match l with
    | [] -> if mini >= maks then [Przedzial(neg_infinity, infinity)]
      else [ Przedzial( neg_infinity, mini ); Przedzial( maks, infinity ) ]
    | Przedzial(x, y) :: t -> if x = neg_infinity then pom t (max mini y) maks
      else pom t mini (min maks x) in
    pom w neg_infinity infinity
      
let plus (a:wartosc) (b:wartosc) =
  kompresuj (dzialanie dodawanie a b)

let razy (a:wartosc) (b:wartosc) =
  (* jeśli mnożymy przez [0,0] to wynikiem jest [0,0] *)
  match a, b with
  | (Przedzial(x,y)::_ , Przedzial(z,w)::_ ) -> 
    if (x = 0. && y = 0.) || (z = 0. && w = 0.) then Przedzial(0.,0.) :: []
    else kompresuj (dzialanie mnozenie a b)
  | _ -> kompresuj (dzialanie mnozenie a b)

let przeciwny (a:wartosc) =
  (* zwraca listę przedziałów przeciwnych *)
  let rec przeciwny_przedzial (a:wartosc) (ak:wartosc) =
    match a with
    | [] -> ak
    | Przedzial(x, y) :: t -> przeciwny_przedzial t (Przedzial(-.y, -.x) :: ak) in
  przeciwny_przedzial a []

let minus (a:wartosc) (b:wartosc) =
  plus a (przeciwny b)

let odwrotny (a:wartosc) =
  (* zwraca listę przedziałów odwrotnych *)
  match a with
  | [] -> []
  | Przedzial(_, a) :: Przedzial(b, _) :: _ ->
    assert(a < b);
    if a = 0. then Przedzial(neg_infinity, 1. /. b) :: []
    else if b = 0. then Przedzial(1. /. a, infinity) :: []
    else if a *. b < 0. then Przedzial(1. /. a, 1. /. b) :: []
    else Przedzial(neg_infinity, 1. /. b) :: Przedzial( 1. /. a, infinity ) :: []
  | Przedzial(a, b) :: [] ->
    let ia = 1. /. a in
    let ib = 1. /. b in
    let inf = infinity in
    let n_inf = neg_infinity in
    assert(a <= b);
    if a = n_inf && b = inf then Przedzial(n_inf, inf) :: []
    else if a = 0. && b = 0. then []
    else if a = 0. then Przedzial(ib, inf) :: []
    else if b = 0. then Przedzial(n_inf, ia) :: []
    else if a *. b < 0. then Przedzial(n_inf, ia) :: Przedzial(ib, inf) :: []
    else Przedzial(ib, ia) :: []

let podzielic (a:wartosc) (b:wartosc) =
  razy a (odwrotny b)
        
