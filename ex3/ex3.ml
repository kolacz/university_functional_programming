(* Zad. 1 *)

let polyval_tail (poly :float list) (x :float) = let rec polyval_aux poly x acc =
                                                   match poly with
                                                   | []    -> acc
                                                   | p::ps -> polyval_aux ps x ((acc *. x) +. p)
                                                 in
                                                 polyval_aux poly x 0.;;

(**********************************************)

let polyval poly x = let polyval_aux x e poly = List.fold_left (fun e p -> (e *. x +. p)) e poly
                     in
                     polyval_aux x 0. poly;;
                           
let () = Printf.printf "2x^2 -2x +2    ; arg=7. : %.2f\n" (polyval [2.; -2.; 2.] 7.);;
let () = Printf.printf "x^10 + 1       ; arg=2. : %.2f\n" (polyval [1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.] 2.);;
let () = Printf.printf "7x^3 +11x -19  ; arg:0. : %.2f\n" (polyval [7.; 0.; 11.; -19.] 0.);;
let () = Printf.printf "10x^3 +10x^2 + 10x +10; arg: 0.1 : %.4f\n" (polyval [10.; 10.; 10.; 10.] 0.1);;

(* Zad. 2 *)

let rec polyval_explicit2 poly x =
  match poly with
  | []    -> 0.
  | p::ps -> p +. x *. (polyval_explicit2 ps x);;

(**********************************************)
           
let polyval_tail2 poly x = let rec polyval_aux poly x acc pow =
                             match poly with
                             | []    -> acc
                             | p::ps -> polyval_aux ps x (acc +. p *. pow) (pow *. x)
                           in
                           polyval_aux poly x 0. 1.;;

(**********************************************)

let polyval2 poly x = let polyval_aux x poly e = List.fold_right (fun p e -> p +. x *. e) poly e
                      in
                      polyval_aux x poly 0.;;

let () = Printf.printf "polyval2 [2.; -2.; 2.] 7. = %.2f\n" (polyval2 [2.; -2.; 2.] 7.);;
let () = Printf.printf "polyval2 [-117.; 124.; 10.; 0.; -42.] 0. = %.2f\n" (polyval2 [-117.; 124.; 10.; 0.; -42.] 0.);;
let () = Printf.printf "polyval2 [0.] 10. = %.2f\n" (polyval2 [0.] 10.);;


(* Zad. 3 *)

let join xs = let rec join_aux xs acc res =
                match xs with
                | []    -> List.rev (acc::res)
                | y::ys -> match acc with
                           | []               -> join_aux ys [y] res
                           | z::zs when y = z -> join_aux ys (y::acc) res
                           | z::zs            -> join_aux ys [y] (acc::res)
              in
              join_aux xs [] [];;

(* join [1;2;1;2;1;2;2;2;2;2];; *)
(* join [1;2;2;5;6;6;6;2;2;0];; *)                

(* Zad. 4 *)

let matrix = [[ 1; 2; 3; 4; 5];
              [ 6; 7; 8; 9;10];
              [11;12;13;14;15];
              [16;17;18;19;20];
              [21;22;23;24;25]];;

let check mat = match mat with
  | r::rs when r <> [] -> List.fold_left (fun b xs -> (List.length xs = List.length r) && b ) true rs
  | _                  -> false;;

(**********************************************)
                        
let flip f y x = f x y;;

let nth_column mat n = List.map (flip List.nth n) mat;;

(**********************************************)

let indices xs = let rec indices_aux xs acc k = match xs with
                   | []    -> List.rev acc
                   | y::ys -> indices_aux ys (k::acc) (k+1)
                 in
                 indices_aux xs [] 0;;

let transpose mat = List.map (nth_column mat) (indices mat);;

(**********************************************)

let rec zip_non_tail list1 list2 = match (list1, list2) with
  | ([], [])       -> []
  | (x::xs, y::ys) -> (x, y) :: zip_non_tail xs ys
  | _              -> failwith "List lengths differ!"
                                                   
let zip list1 list2 = let rec zip_aux list1 list2 acc =
                        match (list1, list2) with
                        | ([], [])       -> List.rev acc
                        | (x::xs, y::ys) -> zip_aux xs ys ((x,y)::acc)
                        | _              -> failwith "List lengths differ!"
                      in
                      zip_aux list1 list2 [];;

let uncurry f (x,y) = f x y

let zipf f xs ys = List.map (uncurry f) (zip xs ys);;

(* zipf (+.) [1.;2.;3.;4.] [5.;6.;7.;8.];; *)
(* zipf (+.) [] [];; *)

(**********************************************)

let mult_vec vec mat = List.map (List.fold_left (+.) 0.) (List.map (zipf ( *. ) vec) (transpose mat));;

(* mult_vec [1.; 1.] [[2.;3.];[4.;0.1]];; *)
(* mult_vec [1.; 2.] [[2.;0.];[4.;5. ]];; *)

let (@@@) m1 m2 = List.map (flip mult_vec m2) m1;;

(* [[1.;2.];[3.;4.]] @@@ [[2.;0.];[4.;5.]];; *)
(* [[1.;0.;0.;0.];[0.;1.;0.;0.];[0.;0.;1.;0.];[0.;0.;0.;1.]] @@@ 
   [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;12.];[13.;14.;15.; 16.]] *)

(* Zad. 5 *)

let concat_pair (x,y) = x @ y

let fst (x, y) = x

let next_perm cond xs = let rec rev_pref cond xs acc =
                          match xs with
                          | []    -> (acc, [])
                          | y::ys -> match acc with
                                     | []                   -> rev_pref cond ys [y]
                                     | z::zs when cond z y  -> rev_pref cond ys (y::acc)
                                     | z::zs                -> (acc, xs)
                        in
                        let rec swap cond (list1, list2) prev =
                          match (list1, list2) with
                          | (_, []) -> (list1, [])
                          | ([], y::ys)    -> 
                          | (x::xs, y::ys) -> 
                          
                        in
                        let pair = rev_pref cond xs []
                        in
                        uncurry (@) (swap cond pair (List.hd (fst pair)));;
                        (* rev_pref cond xs [];; *)
