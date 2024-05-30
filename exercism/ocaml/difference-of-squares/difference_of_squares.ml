(* https://en.wikipedia.org/wiki/Triangular_number *)
let square_of_sum n =
  let t = n * (n + 1) / 2 in
  t * t

(* https://en.wikipedia.org/wiki/Square_pyramidal_number *)
let sum_of_squares n = n * (n + 1) * ((2 * n) + 1) / 6
let difference_of_squares n = square_of_sum n - sum_of_squares n
