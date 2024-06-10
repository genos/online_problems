open Base

let to_dec ~from ~digits =
  if from < 2 then None
  else
    List.fold_until (List.rev digits) ~init:(1, 0)
      ~f:(fun (b, n) d ->
        if d < 0 || d >= from then Stop None
        else Continue (b * from, n + (d * b)))
      ~finish:(Fn.compose Option.some snd)

let from_dec ~target num =
  if target < 2 then None
  else if Int.(equal num 0) then Some [ 0 ]
  else
    let rec go n ns =
      if Int.(equal n 0) then ns else go (n / target) ((n % target) :: ns)
    in
    Some (go num [])

type base = int

let convert_bases ~from ~digits ~target =
  to_dec ~from ~digits |> Option.map ~f:(from_dec ~target) |> Option.join
