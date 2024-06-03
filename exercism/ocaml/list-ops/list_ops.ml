let fold ~init ~f xs =
  let rec go = function
  | ([], acc) -> acc
  | (y::ys, acc)  -> go (ys, f acc y)
  in go (xs, init)

let length xs = fold ~init:0 ~f:(fun n _ -> succ n) xs

let reverse xs = fold ~init:[] ~f:(fun acc x -> x :: acc) xs

let map ~f xs = reverse @@ fold ~init:[] ~f:(fun acc x -> (f x) :: acc) xs

let filter ~f xs = reverse @@ fold ~init:[] ~f:(fun acc x -> if f x then x :: acc else acc) xs

let append xs ys = fold ~init:ys ~f:(fun acc x -> x :: acc) (reverse xs)

let concat xss = fold ~init:[] ~f:append xss
