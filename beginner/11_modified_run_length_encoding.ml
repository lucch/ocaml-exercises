(*
  Modify the result of the previous problem in such a way that if an element has
  no duplicates it is simply copied into the result list. Only elements with
  duplicates are transferred as (N E) lists.
  
  Since OCaml lists are homogeneous, one needs to define a type to hold both
  single elements and sub-lists.

  type 'a rle =
  | One of 'a
  | Many of int * 'a

  # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
  Many (4, "e")]
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode xs =
  let rec encode' acc count = function
  | [] -> acc
  | [y] -> encode' ((count + 1, y) :: acc) 0 []
  | y :: (z :: _ as zzs) ->
      if y = z
        then encode' acc (count + 1) zzs
        else encode' ((count + 1, y) :: acc) 0 zzs
  in encode' [] 0 xs
     |> List.rev
     |> List.map (fun (n, x) -> if n = 1 then One x else Many (n, x))
