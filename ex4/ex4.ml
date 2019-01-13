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
(* a)     *)

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


(* b)   *)

let rev l = let rec rev_aux l acc =
              match l with
              | []      -> acc
              | (x::xs) -> rev_aux xs (x::acc)
            in
            rev_aux l [];;

(* Podziel listę względem ceil((n+1)/2)-ego elementu t na dwie części i rekurencyjnie utwórz z nich poddrzewa zaczepione w t. *)

let rec generate_balanced k = let halve k =
                                let rec halve_aux k m acc =
                                  match (k, m) with
                                  | (t::ts, [])         -> (rev acc, t, ts)
                                  | (t::ts, [_])        -> (rev acc, t, ts)
                                  | (x::xs, y1::y2::ys) -> halve_aux xs ys (x::acc)
                                in
                                halve_aux k k []
                              in
                              match k with
                              | [] -> Leaf
                              |  _ -> let (lt, v, rt) = halve k in
                                      Node( generate_balanced lt, v, generate_balanced rt);;

(* Zad. 3 *)

(* pierwsza reprezentacja *)
type 'a mtree  = MNode of 'a * 'a forest
and  'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;

let m_t1 = MNode( 5, Forest( MNode(1, EmptyForest), EmptyForest));;

let m_t2 = MNode( 1,
                  Forest( MNode(2,
                                Forest( MNode(4, Forest( MNode( 8, EmptyForest),
                                                         EmptyForest)),
                                        Forest( MNode( 5, EmptyForest),
                                                EmptyForest))),
                          Forest( MNode(3, Forest( MNode( 6, EmptyForest),
                                                   Forest( MNode( 7, EmptyForest),
                                                           EmptyForest))),
                                  EmptyForest)));;

let m_t3 = MNode( 6,
                  Forest( MNode(10,
                                Forest( MNode(9, Forest( MNode( 4, EmptyForest),
                                                         EmptyForest)),
                                        Forest( MNode( 2, EmptyForest),
                                                EmptyForest))),
                          Forest( MNode(17, Forest( MNode( 7, EmptyForest),
                                                   Forest( MNode( 3, EmptyForest),
                                                           EmptyForest))),
                                  EmptyForest)));;

let foo x = Printf.printf "%d " x 


let bfs_forest tree f = let rec bfs_aux_t tree f queue =
                          match tree with
                          | MNode(v, forest) -> let _ = f v in
                                                bfs_aux_f forest f queue
                        and bfs_aux_f forest f queue =
                          match forest with
                          | EmptyForest -> if Queue.is_empty queue then ()
                                           else bfs_aux_t (Queue.pop queue) f queue
                          | Forest(tree, forest) -> let _ = Queue.push tree queue in
                                                    bfs_aux_f forest f queue
                        in
                        bfs_aux_t tree f (Queue.create());;


let dfs_forest tree f = let rec dfs_aux_t tree f =
                          match tree with
                          | MNode(v, forest) -> let _ = f v in
                                                dfs_aux_f forest f 
                        and dfs_aux_f forest f =
                          match forest with
                          | EmptyForest -> ()
                          | Forest(tree, forest) -> let _ = dfs_aux_t tree f in
                                                    dfs_aux_f forest f
                        in
                        dfs_aux_t tree f;;

(* druga reprezentacja *)
type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let bfs_list tree f = 

let dfs_list tree f = ()
