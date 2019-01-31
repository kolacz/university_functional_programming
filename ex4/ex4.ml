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

let m_l1 = MTree( 1,
                  [MTree( 2,
                          [MTree( 3, []);
                           MTree( 4, [])]);
                   MTree( 5, [])]);;

let m_l2 = MTree( 1,
                  [MTree( 2,
                          [MTree( 3,
                                  [MTree( 6, []);
                                   MTree( 9, []);
                                   MTree( 12,[])]);
                           MTree( 4, [])]);
                   MTree( 10,
                          [MTree( 7,
                                  [MTree( 15,[]);
                                   MTree( 11,
                                          [MTree( 19, [])]);
                                   MTree( 0, [])]);
                           MTree( 17, [])]);
                   MTree( 5, [])]);;


(* Map *) 
let bfs_lst tree f = let rec children_to_queue lst q =
                       match lst with
                       | []    -> q
                       | c::cs -> let () = Queue.push c q in
                                  children_to_queue cs q
                     in
                     let rec bfs_aux tree f queue =
                       match tree with 
                       | MTree(v, ts) -> let _ = f v and
                                             queue = children_to_queue ts queue
                                         in
                                         if Queue.is_empty queue then ()
                                         else bfs_aux (Queue.pop queue) f queue
                     in
                     bfs_aux tree f (Queue.create());;

(*
let dfs_lst tree f = let rec dfs_aux tree f =
                       match tree with
                       | MTree(v, ts) -> let _ = f v in
                                         dfs_aux_lst ts f
                     and dfs_aux_lst lst f =
                       match lst with
                       | []    -> ()
                       | t::ts -> let _ = dfs_aux t f in
                                  dfs_aux_lst ts f
                     in
                     dfs_aux tree f;; 
*)

let rec dfs_lst tree f = match tree with
  | MTree(v, ts) -> let _ = f v
                    in dfs_aux_lst ts f
and dfs_aux_lst lst f =
  match lst with
  | []    -> ()
  | t::ts -> let _ = dfs_lst t f
             in dfs_aux_lst ts f;;


(* Zad. 4 *)

type formula =   Var of string
               | Neg of formula
               |  Or of formula * formula
               | And of formula * formula;;

type env = (string * bool) list;;  

exception StopIteration of string
let rec next_env (e :env) = match e with
  | []         -> raise (StopIteration "enough")
  | (v, b)::vs -> if b then (v, false)::(next_env vs)
                       else (v,  true)::vs 

let free_vars f = let rec free_vars_aux f vars =
                    match f with
                    |    Var(s) -> if List.mem_assoc s vars then vars
                                   else (s, false)::vars
                    |    Neg(p) -> free_vars_aux p vars
                    |  Or(p, q) -> let vars_p = free_vars_aux p vars
                                   in free_vars_aux q vars_p
                    | And(p, q) -> let vars_p = free_vars_aux p vars
                                   in free_vars_aux q vars_p
                  in
                  free_vars_aux f [];;

(*  x and (y or z) or x or z  *)  
let f1 = Or(And(Var("x"), Or(Var("y"), Var("z"))), Or(Var("x"), Var("z")));;

(* not(x and not x) *)
let f2 = Neg(And(Var("x"), Neg(Var("x"))));;

(* (p => r) and (q => r) and (p or q) => r *) (* prawo dylematu konstrukcyjnego *)
let f3 = Or(And(Var("p"), Neg(Var("q"))),
            Or(And(Var("q"), Neg(Var("r"))),
               Or(And(Neg(Var("p")), Neg(Var("q"))),
                  Var("r"))));;
  
let rec eval f e = match f with
  |    Var(s) -> List.assoc s e
  |    Neg(p) -> not (eval p e)
  |  Or(p, q) -> (eval p e) || (eval q e)
  | And(p, q) -> (eval p e) && (eval q e)

(* all(iter) *)
let is_tautology f = let e = free_vars f in
                     let rec taut_aux f environ =
                       if eval f environ
                       then
                         try taut_aux f (next_env environ)
                         with StopIteration s -> (true, [])
                       else (false, environ)
                     in
                     taut_aux f e;;
                              
(***************************************)

let rec nnf f = match f with
  |       Or(p, q) ->  Or(nnf p, nnf q)
  |      And(p, q) -> And(nnf p, nnf q)
  | Neg( Or(p, q)) -> And(nnf(Neg(p)), nnf(Neg(q)))
  | Neg(And(p, q)) ->  Or(nnf(Neg(p)), nnf(Neg(q)))
  |    Neg(Neg(p)) -> p 
  |              _ -> f

(***************************************)                  

let cnf f = let rec cnf_aux f =
              match f with
              | Or(p, And(q, r)) -> And(cnf_aux(Or(cnf_aux p, cnf_aux q)),
                                        cnf_aux(Or(cnf_aux p, cnf_aux r)))
              | Or(And(p, q), r) -> And(cnf_aux(Or(cnf_aux p, cnf_aux r)),
                                        cnf_aux(Or(cnf_aux q, cnf_aux r)))
              | _ -> f
            in
            cnf_aux (nnf f);;

(* nowy typ cnf -> listy *)
type literal = V of string | NV of string
type norm_formula = literal list list

let rec cnf_to_lists f = let rec clause c acc =
                           match c with
                           | Var(s)      -> if not(List.mem ( V s) acc) then ( V s)::acc else acc
                           | Neg(Var(s)) -> if not(List.mem (NV s) acc) then (NV s)::acc else acc
                           | Or(p, q)    -> let acc1 = clause p acc in
                                            clause q acc1
                         in
                         match f with
                         | Or(p, q) as alt -> [clause alt []] 
                         | And(p, q)       -> cnf_to_lists p @ cnf_to_lists q;;

                     
(*
is_tautology (And(Or(Neg( cnf (Or(And(Var("p"), And(Var("q"), Var("r"))), (Or(Var("p"), And(Var("q"), Var("t"))))))), Or(And(Var("p"), And(Var("q"), Var("r"))), (Or(Var("p"), And(Var("q"), Var("t")))))), Or( cnf (Or(And(Var("p"), And(Var("q"), Var("r"))), (Or(Var("p"), And(Var("q"), Var("t")))))), Neg(Or(And(Var("p"), And(Var("q"), Var("r"))), (Or(Var("p"), And(Var("q"), Var("t")))))))));; 
 *)

(***************************************)

(* popraw *) 
(*let is_tautology_cnf f = let rec tcnf_aux f acc =
                           match f with
                           |             Var(s) -> List.mem (Neg(Var s)) acc
                           |        Neg(Var(s)) -> List.mem (Var s) acc
                           |          And(p, q) -> tcnf_aux p [] && tcnf_aux q []
                           |      Or(Var(s), q) -> if List.mem (Neg(Var s)) acc then true
                                                   else tcnf_aux q ((Var s)::acc)
                           | Or(Neg(Var(s)), q) -> if List.mem (Var s) acc then true
                                                   else tcnf_aux q ((Neg(Var s))::acc)
                           |           Or(p, q) -> tcnf_aux p acc || tcnf_aux q acc
                         in
                         tcnf_aux (cnf f) [];;*)

let is_tautology_cnf f = let neg p =
                           match p with
                           |  V s -> NV s
                           | NV s -> V s 
                         in
                         let rec neg_found p ps =
                           match ps with
                           | []    -> false
                           | l::ls -> if p = neg l then true
                                      else neg_found p ls
                         in
                         let rec tcnf_aux e c =
                           match c with
                           | []    -> false && e
                           | l::ls -> if not(neg_found l ls)
                                      then tcnf_aux e ls else true && e
                         in
                         List.fold_left tcnf_aux true (cnf_to_lists (cnf f));;

(***************************************)

(*let dnf f = let rec dnf_aux f =
              match f with
              | And(p, Or(q, r)) -> Or(dnf_aux(And(cnf_aux p, cnf_aux q)),
                                       dnf_aux(And(cnf_aux p, cnf_aux r)))
              | And(Or(p, q), r) -> Or(dnf_aux(And(cnf_aux p, cnf_aux r)),
                                       dnf_aux(And(cdnf_aux q, cnf_aux r)))
              | _ -> f
            in
            cnf_aux (nnf f);;
*)


(***************************************)

(* Zad. 5 *)

let rec fac_cps n cont = if n == 0 then cont 1
                         else fac_cps (n - 1) (fun v -> cont (v * n));;

(* fac_cps (n - 1) (fun v -> cont (v * n)) =  fac_cps (n - 2) (fun w -> (fun v -> cont (v * n)) (w * (n - 1))) *) 

(* fac_cps 2 (fun x -> x) = fac_cps 1 (fun v -> cont (v * 2)) = 
   fac_cps 0 (fun w -> (fun v -> cont (v * 2)) (w * 1)) =
   (fun w -> (fun v -> cont (v * 2)) (w * 1)) 1 *)

let pow2'  a cont = cont (a ** 2.);;
let add' a b cont = cont (a +. b);;
let sqrt'  a cont = cont (sqrt a);;
let pyth a b cont = pow2' a (fun a2 -> pow2' b (fun b2 -> add' a2 b2 (fun anb -> sqrt' anb cont)));;

(* type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;; *)

let prod tree = let rec prod_cps tree cont =
                  match tree with
                  | Node(Leaf, v, Leaf) -> cont v
                  | Node(Leaf, v, rt)   -> prod_cps rt (fun right -> cont(right * v))
                  | Node(lt, v, Leaf)   -> prod_cps lt (fun left  -> cont(left * v))
                  | Node(lt, v, rt)     -> prod_cps lt (fun left  -> prod_cps rt (fun right -> cont(left * right * v)))
                in
                prod_cps tree (fun x -> x);;

let () = Printf.printf "prod t: %d, fac_cps (max t): %d\n" (prod t) (fac_cps 6 (fun x -> x))
let () = Printf.printf "prod t_balanced %d, fac_cps (max t_balanced): %d\n" (prod t_balanced) (fac_cps 8 (fun x -> x))

(***************************************)

let prod1 tree cont = let rec prod_cps tree cont1 cont2 = 
                        match tree with
                        | Leaf            -> cont1 1
                        | Node(lt, v, rt) -> if v = 0 then cont2 0
                                             else prod_cps lt (fun left ->
                                                      prod_cps rt (fun right ->
                                                          cont1(left * right * v))
                                                        cont2)
                                                    cont2
                      in
                      prod_cps tree cont cont;;

let t2 = Node(
             Node(
                 Node(
                     Node(
                         Leaf, 8, Leaf),
                     4 ,
                     Node(Leaf, 9, Leaf)),
                 2, Node(
                        Node(Leaf, 10,Leaf),
                        0,
                        Node(Leaf, 11, Leaf))),
             1,
             Node(
                 Node(Leaf, 6, Leaf),
                 3,
                 Node(
                     Node(Leaf, 12, Leaf),
                     7,
                     Node(Leaf, 13, Leaf))));;
                           
(***************************************)

(* Zad. 6 *)

