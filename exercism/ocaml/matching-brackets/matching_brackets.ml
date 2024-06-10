let are_balanced =
  let open Base in
  let left c = Char.(equal c '[' || equal c '{' || equal c '(') in
  let right c = Char.(equal c ']' || equal c '}' || equal c ')') in
  let pair = function '[', ']' | '{', '}' | '(', ')' -> true | _ -> false in
  String.fold_until ~init:[]
    ~f:(fun stack c ->
      match (stack, c) with
      | _, y when left y -> Continue (y :: stack)
      | [], y when right y -> Stop false
      | x :: xs, y when pair (x, y) -> Continue xs
      | x :: _, y when right y && not (pair (x, y)) -> Stop false
      | _ -> Continue stack)
    ~finish:List.is_empty
