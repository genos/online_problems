open Base

let max = 26 * 26 * 10 * 10 * 10
let counter = ref 0

type robot = { mutable start : int; mutable current : int }

let alpha n = Stdlib.Char.chr @@ (65 + n)
let num n = Stdlib.Char.chr @@ (48 + n)

let num2name n =
  let _5 = n % 10 in
  let _4 = n / 10 % 10 in
  let _3 = n / (10 * 10) % 10 in
  let _2 = n / (10 * 10 * 10) % 26 in
  let _1 = n / (26 * 10 * 10 * 10) % 26 in
  String.of_list [ alpha _1; alpha _2; num _3; num _4; num _5 ]

let new_robot () =
  let r = { start = !counter; current = !counter } in
  counter := !counter + 1;
  r

let name r =
  let n = num2name r.current in
  r.current <- (r.current + 1) % max;
  n

let reset r =
  counter := !counter + 1;
  r.start <- !counter;
  r.current <- !counter;
