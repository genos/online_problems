let anagrams x =
  let open Base in
  let open String in
  let ( <.> ) = Fn.compose in
  let f = List.sort ~compare:Char.compare <.> to_list <.> lowercase in
  let x' = f x in
  List.filter ~f:(fun y ->
      (not (Caseless.equal x y)) && List.equal Char.equal x' (f y))
