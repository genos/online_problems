open Base
open React

type progress = Win | Lose | Busy of int
type t = (char -> unit) * string signal * progress signal

let step = function Win -> Win | Busy n when n > 0 -> Busy (n - 1) | _ -> Lose

let guess w (m, p) c =
  let ok = ref false in
  let m' =
    String.mapi m ~f:(fun i x ->
        if Char.(equal x '_' && equal w.[i] c) then (
          ok := true;
          c)
        else x)
  in
  let p' =
    if String.equal m' w then Win else if ok.contents then p else step p
  in
  (m', p')

let create w =
  let init = (String.map ~f:(Fn.const '_') w, Busy 9) in
  let guesses, send_guess = E.create () in
  let states = S.fold (guess w) init guesses in
  ((fun c -> send_guess c), S.map fst states, S.map snd states)

let feed c (f, _, _) = f c
let masked_word (_, m, _) = m
let progress (_, _, p) = p
