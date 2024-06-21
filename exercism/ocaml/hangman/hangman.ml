open Base
open React

type t = { word : char array; known : bool array; lives : int ref }
type progress = Win | Lose | Busy of int

let create word =
  {
    word = String.to_array word;
    known = Array.create ~len:(String.length word) false;
    lives = ref 9;
  }

let feed c { word; known; lives } =
  let ok = ref false in
  Array.iteri word ~f:(fun i x ->
      if Char.equal c x then
        if not known.(i) then (
          ok := true;
          known.(i) <- true));
  if not !ok then lives := !lives - 1

let masked_word { word; known; _ } =
  Array.zip_exn word known
  |> Array.map ~f:(fun (c, b) -> if b then c else '_')
  |> String.of_array |> S.const

let progress { known; lives; _ } =
  S.const
    (match (Array.for_all ~f:Fn.id known, !lives) with
    | true, _ -> Win
    | _, n when n < 0 -> Lose
    | _, n -> Busy n)
