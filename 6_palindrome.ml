(*
Find out whether a list is a palindrome.

Hint: A palindrome is its own reverse.   
*)

(* 'a list -> bool *)
let is_palindrome xs =
  let rev =
    let rec rev' acc = function 
    | [] -> acc
    | y :: ys -> rev' (y :: acc) ys
    in  rev' []
  in xs = rev xs

let is_palindrome' xs =
  xs = List.rev xs
