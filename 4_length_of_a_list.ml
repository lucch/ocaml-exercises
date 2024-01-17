(*
  Find the number of elements of a list.

  OCaml standard library has List.length but we ask that you reimplement it.
  Bonus for a tail recursive solution.   
*)

let rec length = function
  | [] -> 0
  | _ :: xs -> 1 + length xs

let length' xs =
  let rec length'' n = function
  | [] -> n
  | y :: ys -> length'' (n + 1) ys
  in length'' 0 xs
