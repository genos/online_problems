type nucleotide = A | C | G | T

let hamming_distance xs ys =
  match (xs, ys) with
  | _ :: _, [] -> Error "right strand must not be empty"
  | [], _ :: _ -> Error "left strand must not be empty"
  | _ -> (
      try
        Result.ok
        @@ List.fold_left2 (fun n x y -> n + Bool.to_int (x != y)) 0 xs ys
      with Invalid_argument _ ->
        Result.error "left and right strands must be of equal length")
