open Base
module Int_map = Map.M (Int)

type school = string list Int_map.t

let sort = List.sort ~compare:String.compare
let oplist g = g |> Option.to_list |> List.concat |> sort
let empty_school = Map.empty (module Int)
let add k i s = Map.update s i ~f:(fun g -> sort (k :: oplist g))
let grade i s = oplist @@ Map.find s i
let sorted = Map.map ~f:sort
let roster = Map.fold_right ~init:[] ~f:(fun ~key:_ ~data g -> data @ g)
