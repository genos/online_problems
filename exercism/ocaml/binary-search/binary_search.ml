open Base

let find xs x =
  let rec go lo hi =
    if lo >= hi then Error "value not in array"
    else
      let m = Int.shift_right lo 1 + Int.shift_right hi 1 in
      match Int.sign @@ compare x xs.(m) with
      | Neg -> go lo m
      | Zero -> Ok m
      | Pos -> go (m + 1) hi
  in
  go 0 (Array.length xs)
