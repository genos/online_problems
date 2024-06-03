open Base

let acronym s =
  s
  |> String.split_on_chars ~on:[ ' '; '-'; '_' ]
  |> List.map ~f:(Fn.flip String.prefix 1)
  |> String.concat |> String.uppercase
