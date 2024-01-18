(*
  Flatten a nested list structure.
*)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let nodes = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]

(*
  The Haskell equivalent would be:

  data Node a
    = One a
    | Many [Node a]

  nodes :: [Node String]
  nodes = [One "a", Many [One "b", Many [One "c", One "d"], One "e"]]
*)

let rec flatten xs =
  let rec flatten' acc = function
  | [] -> acc
  | One y :: ys -> flatten' (List.append acc y) ys
  | Many ys :: zs -> flatten' (List.append acc (flatten ys)) zs
  in flatten' [] xs 

let flatten' xs =
  let rec flatten'' acc = function
  | [] -> acc
  | One y :: ys -> flatten'' (y :: acc) ys
  | Many ys :: zs -> flatten'' (flatten'' acc ys) zs
  in List.rev @@ flatten'' [] xs
