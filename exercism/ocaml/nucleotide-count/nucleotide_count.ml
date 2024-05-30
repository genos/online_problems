open Base

let empty = Map.empty (module Char)

let count_nucleotides s =
  let init =
    Map.of_alist_exn (module Char) [ ('A', 0); ('C', 0); ('G', 0); ('T', 0) ]
  in
  let f m c =
    match c with
    | 'A' | 'C' | 'G' | 'T' -> Ok (Map.change m c ~f:(Option.map ~f:Int.succ))
    | _ -> Error c
  in
  Result.map ~f:(Map.filter ~f:(( < ) 0)) @@ String.fold_result s ~init ~f

let count_nucleotide s c =
  match c with
  | 'A' | 'C' | 'G' | 'T' ->
      Result.map ~f:(fun m -> Option.value ~default:0 @@ Map.find m c)
      @@ count_nucleotides s
  | _ -> Error c
