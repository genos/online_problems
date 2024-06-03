let rec fold ~init ~f = function
  | [] -> init
  | x :: xs -> fold ~init:(f init x) ~f xs

let length xs = fold ~init:0 ~f:(fun n _ -> succ n) xs
let reverse xs = fold ~init:[] ~f:(Fun.flip List.cons) xs
let map ~f xs = fold ~init:[] ~f:(fun ys x -> f x :: ys) (reverse xs)

let filter ~f xs =
  fold ~init:[] ~f:(fun ys x -> if f x then x :: ys else ys) (reverse xs)

let append xs ys = fold ~init:ys ~f:(Fun.flip List.cons) (reverse xs)
let concat xss = fold ~init:[] ~f:append xss
