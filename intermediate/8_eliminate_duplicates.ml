(*
  Eliminate consecutive duplicates of list elements.
*)

let compress xs =
  let rec compress' acc = function
  | [] -> acc
  | [y] -> y :: acc
  | y :: z :: zs ->
      if y = z
        then compress' acc (y :: zs)
        else compress' (y :: acc) (z :: zs)
  in List.rev @@ compress' [] xs

let rec compress' = function
  | x :: (y :: _ as yys) -> if x = y then compress' yys else x :: compress' yys
  | xs -> xs
