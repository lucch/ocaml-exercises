(*
  If you need so, refresh your memory about run-length encoding:
  http://en.wikipedia.org/wiki/Run-length_encoding.

  Here is an example:

  # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : (int * string) list =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]   
*)

(*  ["a"; "a"; "a"]  *)
let encode xs = 
  let rec encode' count = function
  | [] -> []
  | [y] -> [(count + 1, y)]
  | y :: (z :: _ as zzs) ->
      if y = z
        then encode' (count + 1) zzs
        else (count + 1, y) :: encode' 0 zzs
  in encode' 0 xs
