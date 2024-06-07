open Base

type bst = Empty | Node of int * bst * bst

let empty = Empty
let value = function Empty -> Error "value on empty" | Node (v, _, _) -> Ok v
let left = function Empty -> Error "left on empty" | Node (_, l, _) -> Ok l
let right = function Empty -> Error "right on empty" | Node (_, _, r) -> Ok r

let rec insert i b =
  match b with
  | Empty -> Node (i, Empty, Empty)
  | Node (j, l, r) when j >= i -> Node (j, insert i l, r)
  | Node (j, l, r) -> Node (j, l, insert i r)

let to_list b =
  let rec go = function
    | xs, Empty -> xs
    | xs, Node (v, l, r) -> go (v :: go (xs, r), l)
  in
  go ([], b)
