let factors_of n =
  let open Base in
  let open Int64.O in
  if n <= 0L then invalid_arg "Positive ints only"
  else
    let rec go = function
      | 1L, _, fs -> List.rev fs
      | k, f, fs when k % f = 0L -> go (k / f, f, f :: fs)
      | k, f, fs ->
          let f' = if f = 2L then 3L else f + 2L in
          go (k, f', fs)
    in
    go (n, 2L, [])
