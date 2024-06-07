open Base

let is_pangram s =
  let f cs c =
    let open Char in
    let c' = lowercase c in
    if 'a' <= c' && c' <= 'z' then cs.(to_int c' - to_int 'a') <- true;
    cs
  in
  Array.for_all ~f:Fn.id @@ String.fold ~init:(Array.create ~len:26 false) ~f s
