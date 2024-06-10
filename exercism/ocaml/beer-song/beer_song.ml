open Base

let step n = if Int.equal n 0 then 99 else n - 1
let num n = if Int.equal n 0 then "No more" else Int.to_string n
let bots n = if Int.equal n 1 then "bottle" else "bottles"

let act = function
  | 0 -> "Go to the store and buy some more"
  | n -> Printf.sprintf "Take %s down and pass it around" (if Int.equal n 1 then "it" else "one")

let verse n =
  let n' = step n in
  Printf.sprintf "\n%s %s of beer on the wall, %s %s of beer.\n%s, %s %s of beer on the wall."
    (num n) (bots n)
    (String.lowercase @@ num n)
    (bots n) (act n)
    (String.lowercase @@ num n')
    (bots @@ n')

let recite from num =
  List.range (from - num + 1) (from + 1)
  |> List.fold ~init:[] ~f:(fun acc n -> verse n :: acc)
  |> String.concat_lines |> String.strip
