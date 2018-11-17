(* Zad. 1 *)

let palindrom k = let rec halve_aux k m acc =
                    match (k, m) with
                    | (t, [])             -> (acc, t)
                    | (t::ts, [_])        -> (acc, ts)
                    | (x::xs, y1::y2::ys) -> halve_aux xs ys (x::acc)
                  in
                  let (a, b) = halve_aux k k [] in a = b;;

let () = Printf.printf "palindrom [1;2;3;4;5;4;3;2;1]: %b\n" (palindrom [1;2;3;4;5;4;3;2;1]);;
let () = Printf.printf "palindrom [1;2;3;4;5;4;3;1]: %b\n" (palindrom [1;2;3;4;5;4;3;1]);;
let () = Printf.printf "palindrom []: %b\n" (palindrom []);;
let () = Printf.printf "palindrom ['a']: %b\n" (palindrom ['a']);;

(* Zad. 2 *)

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let t = Node( Node( Node( Leaf, 4, Leaf), 2, Node( Node( Leaf, 6, Leaf), 5, Leaf)), 1, Node( Leaf, 3, Leaf));;
let t_balanced = Node( Node( Node( Leaf, 4, Leaf), 2, Node( Node( Leaf, 6, Leaf), 5, Leaf)), 1, Node( Node( Leaf, 7, Leaf), 3, Node(Leaf, 8, Leaf)))

let is_balanced t = let rec vcount tree  =
                      match tree with
                      | Leaf            -> 0
                      | Node(lt, e, rt) -> vcount lt + vcount rt + 1
                    in
                    let rec is_balanced_aux t acc =
                      match t with
                      | Leaf            -> true
                      | Node(lt, e, rt) -> abs((vcount lt) - (vcount rt)) <= 1
                                           && (is_balanced_aux lt acc)
                                           && (is_balanced_aux rt acc)
                    in
                    is_balanced_aux t 0;;

let snd (a, b) = b
let is_balanced2 t = let rec is_balanced_aux t = 
                       match t with
                       | Leaf                -> (0, true)
                       | Node(Leaf, _, Leaf) -> (1, true)
                       | Node(lt, _, rt)     -> let (m, p) = is_balanced_aux lt and
                                                    (n, q) = is_balanced_aux rt in
                                                (m + n + 1, abs(m - n) <= 1 && p && q)
                     in
                     snd(is_balanced_aux t);;

