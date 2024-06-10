open Base

let number s =
  let open Result.Let_syntax in
  let if_ ~f ~e s = if String.exists ~f s then Error e else Ok s in
  ( s
  |> String.filter ~f:(Fn.non Char.is_whitespace)
  |> String.chop_prefix_if_exists ~prefix:"+"
  |> if_ ~f:Char.is_alpha ~e:"letters not permitted"
  >>= if_
        ~f:(fun c -> not Char.(c = '(' || c = ')' || c = '-' || c = '.' || is_digit c))
        ~e:"punctuations not permitted"
  >>| String.filter ~f:Char.is_digit
  >>= fun x ->
    match (String.length x, Char.(String.get x 0 = '1')) with
    | n, _ when n < 10 -> Error "incorrect number of digits"
    | n, _ when n > 11 -> Error "more than 11 digits"
    | 11, false -> Error "11 digits must start with 1"
    | 11, true -> Ok (String.chop_prefix_exn ~prefix:"1" x)
    | _, _ -> Ok x )
  >>= fun x ->
  match (x.[0], x.[3]) with
  | '0', _ -> Error "area code cannot start with zero"
  | '1', _ -> Error "area code cannot start with one"
  | _, '0' -> Error "exchange code cannot start with zero"
  | _, '1' -> Error "exchange code cannot start with one"
  | _ -> Ok x
