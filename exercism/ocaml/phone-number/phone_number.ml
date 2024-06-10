open Base

let if_chr ~f ~e s = if String.exists ~f s then Error e else Ok s

let number s =
  let open Result.Let_syntax in
  s
  |> String.filter ~f:(Fn.non Char.is_whitespace)
  |> String.chop_prefix_if_exists ~prefix:"+"
  |> if_chr ~f:Char.is_alpha ~e:"letters not permitted"
  >>= if_chr
        ~f:(fun c ->
          not Char.(c = '(' || c = ')' || c = '-' || c = '.' || is_digit c))
        ~e:"punctuations not permitted"
  >>| String.filter ~f:Char.is_digit
  >>= fun x ->
  let n = String.length x in
  let one = Char.(String.get x 0 = '1') in
  if n < 10 then Error "incorrect number of digits"
  else if n > 11 then Error "more than 11 digits"
  else if n = 11 && not one then Error "11 digits must start with 1"
  else
    return (if n = 10 then x else String.chop_prefix_if_exists ~prefix:"1" x)
    >>| String.to_list
    >>= function
    | '0' :: _ -> Error "area code cannot start with zero"
    | '1' :: _ -> Error "area code cannot start with one"
    | _ :: _ :: _ :: '0' :: _ -> Error "exchange code cannot start with zero"
    | _ :: _ :: _ :: '1' :: _ -> Error "exchange code cannot start with one"
    | x -> Ok x >>| String.of_list
