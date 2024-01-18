(*
  Pack consecutive duplicates of list elements into sublists.   
*)

let pack xs = 
  let rec pack' acc1 acc2 = function
  | [] -> acc2
  | [y] -> (y :: acc1) :: acc2
  | y :: (z :: _ as zzs) -> 
      if y = z
        then pack' (y :: acc1) acc2 zzs
        else pack' [] ((y :: acc1) :: acc2) zzs
  in List.rev @@ pack' [] [] xs

let pack' xs =
  let rec pack'' acc = function
  | [] -> [acc]
  | [x] -> [x :: acc]
  | y :: (z :: _ as zzs) ->
      if y = z
        then pack'' (y :: acc) zzs
        else (y :: acc) :: pack'' [] zzs
  in match xs with
  | [] -> []
  | _ -> pack'' [] xs

let rec pack'' = function
  | [] -> []
  | xs ->
    let rec consecutive = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | y :: (z :: _ as zzs) ->
        if y = z
          then
            let (res, t) = consecutive zzs
            in (y :: res, t)
          else ([y], zzs)
    in match consecutive xs with
    | (ys, []) -> [ys]
    | (ys, zs) -> ys :: pack'' zs
