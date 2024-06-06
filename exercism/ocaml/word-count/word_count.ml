open Base

let word_count s =
  s
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; ',' ]
  |> List.map ~f:(String.strip ~drop:(Fn.non Char.is_alphanum))
  |> List.filter_map ~f:(fun w ->
         if String.is_empty w then None else Some (String.lowercase w, 1))
  |> Map.of_alist_fold (module String) ~init:0 ~f:( + )
