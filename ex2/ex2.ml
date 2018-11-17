(***
let rec unzip ps = match ps with
  | []          -> ([], [])
  | (h1, h2)::t -> let (l1, l2) = unzip t in (h1::l1, h2::l2);; 
 ***)

(* Zad. 1 *)

let rec map f l = match l with
  | []    -> []
  | x::xs -> f x :: map f xs;;


let rec sublists l = match l with
  | [] -> [[]]
  | x::xs -> let t = sublists xs in
             t @ (map ((@)[x]) t);;

(* Zad. 2 *)

let reverse l = let rec rev_aux l acc =
                       match l with
                       | []    -> acc
                       | x::xs -> rev_aux xs (x::acc) in
                     rev_aux l [];;

let cycle_clockwise l n = let rec cycle_aux l acc n =
                            if n = 0 then l @ (reverse acc)
                            else match l with
                                 | []    -> reverse acc
                                 | x::xs -> cycle_aux xs (x::acc) (n-1) in
                          cycle_aux l [] n;;

let cycle l n = let t = reverse l in
                let rec cycle_aux l acc n =
                  if n = 0 then acc @ (reverse l)
                  else match l with
                       | []    -> acc
                       | x::xs -> cycle_aux xs (x::acc) (n-1) in
                cycle_aux t [] n;;

(* cycle [1;2;3;4] 1;;   - : int list = [4; 1; 2; 3]  *)
(* cycle [1;2;3;4] 3;;   - : int list = [2; 3; 4; 1]  *)
(* cycle [1;2;3;4] 10;;  - : int list = [1; 2; 3; 4]  *)

(* Zad. 3 *)

let rec merge cmp l1 l2 = match (l1, l2) with
  | (t, [])        -> t
  | ([], t)        -> t
  | (x::xs, y::ys) -> if cmp x y then x :: (merge cmp xs l2)
                                 else y :: (merge cmp l1 ys);;

(* merge (<=) [1;2;3;4;5;6] [1;1;2;2;2;2;2;2;2];; *)

(*** w mergesorcie b�dziemy mieli prawie r�wne po��wki, wi�c w merge_aux nie op�aca si�
     appendowa� 't' do odwr�conego akumulatora. lepiej doklei� z przodu te elementy i odwr�ci� 'ca��' list�
 ***)

let merge_tail cmp l1 l2 = let rec push_front f acc =  
                             match f with
                             | []    -> acc
                             | x::xs -> push_front xs (x::acc)
                           in
                           let rec merge_aux cmp l1 l2 acc =
                             match (l1, l2) with
                             | (t, [])        -> reverse (push_front t acc)
                             | ([], t)        -> reverse (push_front t acc)
                             | (x::xs, y::ys) -> if cmp x y then merge_aux cmp xs l2 (x::acc)
                                                            else merge_aux cmp l1 ys (y::acc)
                           in merge_aux cmp l1 l2 [];;
                           
                                              
let range n = let rec range_aux n count acc =
                if n = 0 then reverse acc
                else range_aux (n-1) (count+1) (count::acc)
              in
              range_aux n 1 [];;

(* merge      (<=) (map ((+) 1) (range 100000)) (range 100000);; *)
(* merge_tail (<=) (map ((+) 1) (range 100000)) (range 100000);; *)

let length l = let rec length_aux l acc =
                 match l with
                 | []    -> acc
                 | x::xs -> length_aux xs (acc+1)
               in
               length_aux l 0;;


let rec mergesort l = let halve k =
                        let rec halve_aux k m acc =
                          match (k, m) with
                          | (t, [] ) -> (reverse acc, t)
                          | (t, [_]) -> (reverse acc, t)
                          | (x::xs, y1::y2::ys) -> halve_aux xs ys (x::acc)
                        in
                        halve_aux k k []
                      in
                      match halve l with
                      | ([], t)  -> t
                      | (h1, h2) -> merge_tail (<=) (mergesort h1) (mergesort h2);;


(* mergesort (map ((-) 100000) (range 100000));; *)

let rec merge_tail_2 cmp l1 l2 = let rec merge_aux cmp l1 l2 acc =
                                   match (l1, l2) with
                                   | (t, [])        -> acc @ t
                                   | ([], t)        -> acc @ t 
                                   | (x::xs, y::ys) -> if cmp x y then merge_aux cmp xs l2 (acc @ [x])
                                                       else merge_aux cmp l1 ys (acc @ [y])
                                 in merge_aux cmp l1 l2 [];;

let rec mergesort_2 l = let halve k =
                        let rec halve_aux k m acc =
                          match (k, m) with
                          | (t, [] ) -> (reverse acc, t)
                          | (t, [_]) -> (reverse acc, t)
                          | (x::xs, y1::y2::ys) -> halve_aux xs ys (x::acc)
                        in
                        halve_aux k k []
                      in
                      match halve l with
                      | ([], t)  -> t
                      | (h1, h2) -> merge_tail_2 (<=) (mergesort h1) (mergesort h2);;

(***

let start = Sys.time();;

mergesort (map ((-) 100000) (range 100000));; 
              
Printf.printf "mergesort 1: %.5f" (Sys.time() -. start)  (* mergesort 1: 0.2070000000- : unit = () *) ;;

let start2 = Sys.time();;

mergesort_2 (map ((-) 100000) (range 100000));; 

Printf.printf "mergesort 2: %.5f" (Sys.time() -. start2) (* mergesort 2: 61.6010000000- : unit = () *) ;;

 ***)

(* Zad. 4 *)

let rec partition cond l = match l with
  | []    -> ([], [])
  | x::xs -> let (t1, t2) = partition cond xs in
             if cond x then (x::t1, t2)
             else (t1, x::t2);;

let partition2 cond l = let rec partition_aux cond l lacc racc =
                          match l with
                          | []    -> (reverse lacc, reverse racc)
                          | x::xs -> if cond x then partition_aux cond xs (x::lacc) racc
                                     else partition_aux cond xs lacc (x::racc)
                        in
                        partition_aux cond l [] [];;

let rec quicksort cmp l = match l with
  | []    -> []
  | x::xs -> let (h1, h2) = partition2 (cmp x) xs in
             (quicksort cmp h1) @ [x] @ (quicksort cmp h2);;                   
             
(* quicksort (>=) [65152;1;9;5;2;7;3;5];; *)

(***

# quicksort (>=) [1];;
- : int list = [1]
# quicksort (>=) [];;
- : 'a list = []

 ***)

(* Zad. 5 *)

let insert x l = let rec insert_aux x l acc =
                   match l with
                   | []    -> [acc @ [x]]
                   | y::ys -> (acc @ [x] @ l) :: insert_aux x ys (acc @ [y])
                 in
                 insert_aux x l [];;

let rec concatmap f l = match l with
  | []    -> []
  | x::xs -> (f x) @ (concatmap f xs)
     
let rec perm l = match l with
  | []    -> [[]]
  | x::xs -> concatmap (insert x) (perm xs)
           
(* Zad. 6 *)

let rec suffixes l = match l with
  | []    -> [[]]
  | x::xs -> l :: suffixes xs;;

let rec scanl f e l = match l with
  | []    -> [e]
  | x::xs -> e :: (scanl f (f e [x]) xs)

let preffixes l = scanl (@) [] l
