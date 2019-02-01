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
  | LNil                   -> failwith "Sacrebleu!"
  | LCons(x, xs) as stream -> LCons(f (ltake(3, stream)), fun () -> map3 f (xs()))

let eulers_transform = function
  | x::y::z::[] -> z -. (y -. z)**2. /. (x -. 2. *. y +. z)
  | _           -> 22. /. 7.

(* Module Lazy *)

open Lazy

type 'a lazylist = LNil | LCons of 'a * 'a lazylist Lazy.t

let rec lazy_take = function
  | (0, _)                 -> []
  | (_, LNil)              -> []
  | (k, LCons(x, lazy xs)) -> x :: (lazy_take (k - 1, xs))

let lazy_pi = let rec pi_aux x k =
                LCons(4. *. x, lazy(pi_aux (x +. 1. /. (2. *. k +. 1.) *. (-1.) ** k)
                                 (k +. 1.))) 
              in
              pi_aux 1. 1.;;


let rec lazy_map3 f = function
  | LNil                        -> failwith "Sacrebleu!"
  | LCons(x, lazy xs) as stream -> LCons(f (lazy_take(3, stream)), lazy(lazy_map3 f xs))

let lazy_eulers_transform = function
  | x::y::z::[] -> z -. (y -. z)**2. /. (x -. 2. *. y +. z)
  | _           -> 22. /. 7.;;

                                          
(* Zad. 2 *)

(* nsols : (int list * int) -> int -> move list list *)

type state = S of int list * int list
type move =
  | FILL of int
  | DRAIN of int
  | TRANSFER of int * int

let act state = let (capacities, amounts) = state
                in let rec alter k f st =
                     match st with
                     | (v::vs, w::ws) -> if k = 0 then (v::vs, (f v)::ws)
                                         else let (vs2, ws2) = alter (k - 1) f (vs, ws)
                                              in (v::vs2, w::ws2)
                   in let rec nth_pair n st =
                        match st with
                        | (v::vs, w::ws) -> if n = 0 then (v, w)
                                            else nth_pair (n - 1) (vs, ws)
                      in function
                      | FILL x          -> alter x (fun y -> y) state 
                      | DRAIN x         -> alter x (fun y -> 0) state
                      | TRANSFER (s, t) -> let (t1, t2) = nth_pair t state
                                           in let liters = min (List.nth amounts s) (t1 - t2)
                                              in let rec transfer amount caps s_idx t_idx =
                                                   match caps with
                                                   | c::cs -> if s_idx < t_idx then
                                                                if s_idx = 0 then
                                                                  (c - amount) :: (transfer amount cs (s_idx - 1) (t_idx - 1))
                                                                else if t_idx = 0 then
                                                                  (c + amount) :: cs
                                                                else c :: (transfer amount cs (s_idx - 1) (t_idx - 1))
                                                              else transfer (-amount) caps t_idx s_idx
                                                 in
                                                 (capacities, transfer liters amounts s t);;

(* nsols : (int list * int) -> int -> move list list *)
let cons a b = (a, b)
             
let possible_moves glasses = let rec range m acc = function
                               | []    -> List.rev acc
                               | x::xs -> range (m+1) (m::acc) xs 
                             in let r = range 0 [] glasses
                                in
                                let rec pairs acc = function
                                  | []    -> acc
                                  | x::xs -> let a1 = List.map (cons x) xs
                                             in pairs (a1@acc) xs
                                in let p = pairs [] r
                                   in let rec generate_transfers acc = function
                                        | []         -> acc
                                        | (a, b)::xs -> generate_transfers ((TRANSFER(a, b))::(TRANSFER(b, a))::acc) xs
                                      in
                                      let rec generate_fill_drain acc = function
                                        | []    -> acc
                                        | x::xs -> generate_fill_drain ((FILL x)::(DRAIN x)::acc) xs
                                      in
                                      (generate_transfers [] p) @ (generate_fill_drain [] r)
                                 
                             
(* nsols : (int list * int) -> int -> move list list *)
                                      
let nsols (glasses, volume) n = let amounts = List.map (fun x -> 0) glasses
                                and moves = possible_moves glasses
                                in
                                let state_zero = (glasses, amounts)
                                in let a1 = List.map (act state_zero) moves
                                in a1
                                     
                                
