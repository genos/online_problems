let transform old_format =
  let open Base in
  old_format
  |> List.concat_map ~f:(fun (s, cs) ->
         List.map ~f:(fun c -> (Char.lowercase c, s)) cs)
  |> List.sort ~compare:(fun (c0, _) (c1, _) -> Char.compare c0 c1)
