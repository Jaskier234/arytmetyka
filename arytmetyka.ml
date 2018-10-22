
type wartosc = Wartosc of float * float * wartosc
  
let wartosc_dokladnosc x p =
  let delta = abs_float(p /. 100. *. x) in
  Wartosc(x -. delta , x +. delta)

let wartosc_od_do x y = Wartosc(x,y)

let wartosc_dokladna x = Wartosc(x,x)

let in_wartosc (w:wartosc) (a:float) =
  let Wartosc(x, y) = w in
  if x >= a && y <= a then true
  else false
    
let min_wartosc w =
  let Wartosc(x, y) = w in
  x

let max_wartosc w =
  let Wartosc(x, y) = w in
  y

let sr_wartosc w =
  let Wartosc(x, y) = w in
  if x = neg_infinity || y = infinity then nan
  else (x +. y) /. 2.
 
let () = assert(1 <= 2)


