(*
  N'th Element of a List  

  Remark: OCaml has List.nth which numbers elements from 0 and raises an
  exception if the index is out of bounds.
*)

let rec nth xs n =
  match xs with
  | [] -> None
  | y :: ys -> if n = 0 then Some y else nth ys (n - 1)

let rec nth' xs n =
  match nth xs n with
  | None -> raise @@ Failure "nth'"
  | Some x -> x
