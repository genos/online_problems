open Base

let fs =
  let make k s n = if n % k = 0 then Some s else None in
  [ make 3 "Pling"; make 5 "Plang"; make 7 "Plong" ]

let raindrop n =
  let s = String.concat @@ List.filter_map ~f:(fun f -> f n) fs in
  if String.is_empty s then Int.to_string n else s
