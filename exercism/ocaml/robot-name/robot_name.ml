open Base

let max = 26 * 26 * 10 * 10 * 10

let permutation =
  let create () =
    let ids = Array.init ~f:Fn.id max in
    Array.permute ids;
    ids
  in
  create ()

let global = ref 0
let step () = global := (!global + 1) % max
let read () = permutation.(!global)

type robot = { mutable counter : int }

let alpha n = Char.of_int_exn (65 + n)
let num n = Char.of_int_exn (48 + n)

let num2name n =
  let _5 = n % 10 in
  let _4 = n / 10 % 10 in
  let _3 = n / (10 * 10) % 10 in
  let _2 = n / (10 * 10 * 10) % 26 in
  let _1 = n / (26 * 10 * 10 * 10) % 26 in
  String.of_list [ alpha _1; alpha _2; num _3; num _4; num _5 ]

let new_robot () =
  step ();
  { counter = read () }

let name r =
  let n = num2name r.counter in
  r.counter <- (r.counter + 1) % max;
  n

let reset r =
  step ();
  r.counter <- read ()
