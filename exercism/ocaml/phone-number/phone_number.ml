open Base

let number s =
  let open Result.Let_syntax in
  let if_ ~f ~e s = if String.exists ~f s then Error e else Ok s in
  s
  |> String.filter ~f:(Fn.non Char.is_whitespace)
  |> String.chop_prefix_if_exists ~prefix:"+"
  |> if_ ~f:Char.is_alpha ~e:"letters not permitted"
  >>= if_
        ~f:(fun c -> not Char.(c = '(' || c = ')' || c = '-' || c = '.' || is_digit c))
        ~e:"punctuations not permitted"
  >>| String.filter ~f:Char.is_digit
  >>| (fun x -> (x, String.length x, Char.(String.get x 0 = '1')))
  >>= (function
        | _, n, _ when n < 10 -> Error "incorrect number of digits"
        | _, n, _ when n > 11 -> Error "more than 11 digits"
        | _, 11, false -> Error "11 digits must start with 1"
        | x, 11, true -> Ok (String.chop_prefix_exn ~prefix:"1" x)
        | x, _, _ -> Ok x)
  >>| String.to_list
  >>= function
  | '0' :: _ -> Error "area code cannot start with zero"
  | '1' :: _ -> Error "area code cannot start with one"
  | _ :: _ :: _ :: '0' :: _ -> Error "exchange code cannot start with zero"
  | _ :: _ :: _ :: '1' :: _ -> Error "exchange code cannot start with one"
  | x -> Ok x >>| String.of_list
