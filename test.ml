open Arytmetyka

let p = wartosc_od_do 0.1 2.9;;
let eps = 0.000000000000001;;
let maly = wartosc_od_do 29.34 29.34;;
let duzy = wartosc_od_do 1000000.0 29000000.0 ;;
    

let a = wartosc_dokladnosc 10.0 10.0;;
let b = wartosc_dokladna 7.00;;
let ab = plus a b;;


let bm = wartosc_od_do (-2.0) 3.0;;
let ba = minus ab bm;;

let srednia  = sr_wartosc ba;;

let e = wartosc_dokladna 1.0;;
let bae = razy ba e;;

let asd = razy ba bm;;

let dsa = podzielic ba bm;;

let mini_ba = min_wartosc ba;;
let maks_ba = max_wartosc ba;;
let mini_bm = min_wartosc bm;;
let maks_bm = max_wartosc bm;;
let mini_dsa = min_wartosc dsa;;
let maks_dsa = max_wartosc dsa;;
let ans = sr_wartosc dsa;;

assert(sr_wartosc p = 1.5);
assert(min_wartosc p = 0.1);
assert(max_wartosc p = 2.9);
assert(in_wartosc p 0. = false);
assert(in_wartosc p 0.1 = true);
assert(in_wartosc p 2.9 = true);
assert(in_wartosc p (2.9 +. eps) = false);


assert(in_wartosc duzy (29000000.0 +. eps*.10000000.0) = false);
assert(in_wartosc duzy (1000000.0 -. eps*.1000000.0) = false);
assert(in_wartosc duzy 2564837.24452 = true);
assert(sr_wartosc duzy = 15000000.);



assert(sr_wartosc maly = 29.34);
assert(min_wartosc maly = 29.34);
assert(max_wartosc maly = 29.34);


  
assert( in_wartosc ab 16.00 = true);
assert( in_wartosc ab 10.0 = false);
assert( in_wartosc ab 18.01 = false);


assert( in_wartosc ba 13.0 = true);
assert( in_wartosc ba 7.0 = false);
assert( in_wartosc ba (13. -. eps) = false);
assert( min_wartosc ba = 13.0 );
assert( max_wartosc ba = 20.0 );
assert( sr_wartosc ba = 16.5);



assert( in_wartosc bae 13.0 = true);
assert( in_wartosc bae 7.0 = false);
assert( in_wartosc bae (13. -. eps *. 10.) = false);
assert( sr_wartosc bae = 16.5);



assert( sr_wartosc asd = 10.0);
assert( in_wartosc asd 10.0 = true);
assert( in_wartosc asd (-40.01) = false);



assert( classify_float(sr_wartosc dsa) = classify_float(nan) );
assert( in_wartosc dsa 0.0 = false);
assert( in_wartosc dsa (-6.5) = true);
assert( in_wartosc dsa 4.33 = false);
assert( in_wartosc dsa 234852345234542325.4353435243 = true);

assert( min_wartosc [] = nan);
assert( max_wartosc [] = nan);
assert( sr_wartosc [] = nan);
print_string "OK";;
