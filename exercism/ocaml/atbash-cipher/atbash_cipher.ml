open Base

let normalize = Fn.compose (String.filter ~f:Char.is_alphanum) String.lowercase
let atbash c = Char.(if is_alpha c then to_int 'a' + to_int 'z' - to_int c |> of_int_exn else c)

let encode ?(block_size = 5) plain =
  normalize plain |> String.map ~f:atbash |> String.to_list |> List.chunks_of ~length:block_size
  |> List.map ~f:String.of_list |> String.concat ~sep:" "

let decode = Fn.compose normalize (String.map ~f:atbash)
