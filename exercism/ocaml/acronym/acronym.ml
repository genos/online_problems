open Base

let acronym s =
  s
  |> String.filter_map ~f:(function
       | ' ' | '-' -> Some ' '
       | c -> Option.some_if (Char.is_alpha c) c)
  |> String.split ~on:' '
  |> List.map ~f:(Fn.flip String.prefix 1)
  |> String.concat |> String.uppercase
