module type ELEMENT = sig
  type t

  val compare : t -> t -> int
end

module Make (El : ELEMENT) = struct
  type t = El.t list
  type el = El.t

  let non p x = not @@ p x
  let el_equal x y = 0 = El.compare x y
  let is_empty = List.is_empty
  let is_member xs x = List.mem x xs
  let is_subset xs ys = List.for_all (is_member ys) xs
  let is_disjoint xs ys = List.for_all (non @@ is_member ys) xs
  let equal = List.equal el_equal
  let of_list = List.sort_uniq El.compare
  let add xs x = of_list @@ (x :: xs)

  type status = [ `Both | `OnlyA | `OnlyB ]

  let diff_filter _p _xs _ys = failwith "todo"
  let difference xs ys = List.filter (non @@ is_member ys) xs
  let intersect xs = List.filter (is_member xs)
  let union xs ys = of_list @@ xs @ ys
end
