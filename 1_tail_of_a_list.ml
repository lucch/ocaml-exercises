(*
  Write a function last : 'a list -> 'a option that returns the last element of
  a list
*)

let rec last (xs : 'a list) =
  match xs with
  | [] -> None
  | [x] -> Some x
  | (_ :: xs) -> last xs

let last' xs =
  let hd' = function
  | [] -> None
  | (y :: _) -> Some y
  in hd' (List.rev xs)
