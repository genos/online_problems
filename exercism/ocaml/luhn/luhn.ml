open Base

let s2ds s =
  String.fold_until s ~init:[]
    ~f:(fun ds -> function
      | ' ' -> Continue ds
      | c when Char.('0' <= c && c <= '9') -> Continue Char.((to_int c - to_int '0') :: ds)
      | _ -> Stop None)
    ~finish:Option.some

let luhn n =
  let n' = n + n in
  if n' > 9 then n' - 9 else n'

let valid s =
  match s2ds s with
  | None | Some ([] | [ _ ]) -> false
  | Some ns ->
      Int.(
        equal 0
          (List.chunks_of ~length:2 ns
          |> List.concat_map ~f:(function [ i; j ] -> [ i; luhn j ] | ks -> ks)
          |> List.fold ~init:0 ~f:( + ) |> Fn.flip rem 10))
