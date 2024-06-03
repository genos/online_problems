module type ELEMENT = sig
  type t

  val compare : t -> t -> int
end

module Make (El : ELEMENT) = struct
  type t = El.t list
  type el = El.t

  let el_equal x y = 0 = El.compare x y
  let is_empty = List.is_empty
  let is_member xs x = List.mem x xs
  let is_subset xs ys = List.fold_left (fun b x -> b && is_member ys x) true xs

  let is_disjoint xs ys =
    let all_not_in ls rs =
      List.fold_left (fun b r -> b && (not @@ is_member ls r)) true rs
    in
    all_not_in xs ys && all_not_in ys xs

  let equal = List.equal el_equal
  let of_list xs = List.sort_uniq El.compare xs
  let add xs x = of_list @@ (x :: xs)
  let difference xs ys = List.filter (fun x -> not @@ is_member ys x) xs
  let intersect xs = List.filter (is_member xs)
  let union xs ys = of_list @@ List.append xs ys
end
