(*
  Reverse a list.
  
  OCaml standard library has List.rev but we ask that you reimplement it.   
*)

let rec rev = function
  | [] -> []
  | x :: xs -> List.append (rev xs) [x]

let rev' xs =
  let rec rev'' acc = function
  | [] -> acc
  | y :: ys -> rev'' (y :: acc) ys
  in rev'' [] xs 
