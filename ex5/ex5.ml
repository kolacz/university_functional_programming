(* Zad. 1 *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

(* warm-up *)

let lhd = function
  | LNil        -> failwith "lhd"
  | LCons(x, _) -> x

let ltl = function
  | LNil         -> failwith "ltl"
  | LCons(_, tl) -> tl()

let rec lfrom k = LCons(k, fun () -> lfrom (k + 1))

let rec ltake = function
  | (0, _)             -> []
  | (_, LNil)          -> []
  | (k, LCons(x, lxs)) -> x::(ltake (k - 1, lxs()))

let rec toLazyList = function
  | []    -> LNil
  | x::xs -> LCons(x, fun () -> toLazyList xs)

let rec (@$) ll1 ll2 = match ll1 with
  | LNil         -> ll2
  | LCons(x, xs) -> LCons(x, fun () -> xs() @$ ll2) 

let rec lmap f = function
  | LNil -> LNil
  | LCons(x, xs) -> LCons(f x, fun () -> lmap f (xs()))

(******************************)

let pi = let rec pi_aux x k =
           LCons(4. *. x, fun () -> pi_aux (x +. 1. /. (2. *. k +. 1.) *. (-1.) ** k)
                                      (k +. 1.)) 
         in
         pi_aux 1. 1.;;


let rec map3 f = function
  | LNil -> failwith "Sacrebleu!"
  | LCons(x, xs) as stream -> LCons(f (ltake(3, stream)), fun () -> map3 f (xs()))

let eulers_transform = function
  | x::y::z::[] -> z -. (y -. z)**2. /. (x -. 2. *. y +. z)
  | _           -> 22. /. 7.

(* Module Lazy *)

open Lazy

       
