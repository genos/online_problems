open Base

let normalize = String.filter_map ~f:(fun c -> Char.(Option.some_if (is_alphanum c) (lowercase c)))

let table =
  let a = "abcdefghijklmnopqrstuvwxyz" in
  Map.of_alist_exn (module Char) @@ List.zip_exn (String.to_list a) String.(to_list @@ rev a)

let atbash c = Map.find table c |> Option.value ~default:c

let encode ?block_size plain =
  normalize plain |> String.map ~f:atbash |> String.to_list
  |> List.chunks_of ~length:(Option.value block_size ~default:5)
  |> List.map ~f:String.of_list |> String.concat ~sep:" "

let decode cipher = normalize cipher |> String.map ~f:atbash
