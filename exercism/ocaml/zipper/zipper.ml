open Base

type 'a t = T of 'a Tree.t | L of 'a t * 'a Tree.t | R of 'a t * 'a Tree.t
[@@deriving sexp]

let get_ = function T t -> t | L (_, t) -> t | R (_, t) -> t
let equal veq x y = Tree.equal veq (get_ x) (get_ y)
let of_tree t = T t
let value z = (get_ z).value
let left z = Option.map (get_ z).left ~f:(fun t -> L (z, t))
let right z = Option.map (get_ z).right ~f:(fun t -> R (z, t))

let rec to_tree z = match up z with Some z' -> to_tree z' | None -> get_ z
and up = function T _ -> None | L (z, _) | R (z, _) -> Some z

let rec set_ f = function
  | T t -> T (f t)
  | L (z, t) -> set_left (Some (f t)) z
  | R (z, t) -> set_right (Some (f t)) z

and set_value v = set_ (fun t -> { t with value = v })
and set_left l = set_ (fun t -> { t with left = l })
and set_right r = set_ (fun t -> { t with right = r })
