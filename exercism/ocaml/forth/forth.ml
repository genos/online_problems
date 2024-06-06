open Base

type word = Num of int | Str of string
type stack = int list
type item = Builtin of (stack -> stack option) | User of word list

let word_of_string s = try Num (Int.of_string s) with _ -> Str s
let def name words env = Map.change env name ~f:(fun _ -> Some words)
let lookup env = function Num n -> Some n | Str s -> Map.find env s

let default_env = Map.of_alist_exn (module String) [
    ("+",    Builtin (function [] -> None | [_] -> None | x :: y :: zs -> Some (y + x :: zs)));
    ("-",    Builtin (function [] -> None | [_] -> None | x :: y :: zs -> Some (y - x :: zs)));
    ("*",    Builtin (function [] -> None | [_] -> None | x :: y :: zs -> Some (y * x :: zs)));
    ("/",    Builtin (function [] -> None | [_] -> None | 0 :: _ -> None | x :: y :: zs -> Some (y / x :: zs)));
    ("dup",  Builtin (function [] -> None | x :: xs -> Some (x :: x :: xs)));
    ("drop", Builtin (function [] -> None | _ :: xs -> Some xs));
    ("swap", Builtin (function [] -> None | [_] -> None | x :: y :: zs -> Some (y :: x :: zs)));
    ("over", Builtin (function [] -> None | [_] -> None | x :: y :: zs -> Some (y :: x :: y :: zs)))]  [@@ocamlformat "disable"]

let rec app stack env = function
  | Num n -> Some (n :: stack)
  | Str s -> (
      match Map.find env s with
      | None -> None
      | Some (Builtin f) -> f stack
      | Some (User words) ->
          List.fold_until words ~init:stack
            ~f:(fun s w ->
              match app s env w with
              | None -> Stop None
              | Some s' -> Continue s')
            ~finish:Option.some)

let handle stack env line =
  let words =
    line
    |> String.strip ~drop:(fun c -> Char.(equal c ';' || equal c ':'))
    |> String.lowercase |> String.split ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:word_of_string
  in

  if String.is_prefix line ~prefix:":" && String.is_suffix line ~suffix:";" then
    match words with
    | [] -> None
    | [ _ ] -> None
    | Num _ :: _ -> None
    | Str n :: ws ->
        let e = def n (User ws) env in
        Some (stack, e)
  else
    List.fold_until words ~init:(stack, env)
      ~f:(fun (s, e) w ->
        match w with
        | Num i -> Continue (i :: s, e)
        | Str k -> (
            match app s e (Str k) with
            | None -> Stop None
            | Some s' -> Continue (s', e)))
      ~finish:Option.some

let evaluate =
  List.fold_until ~init:([], default_env)
    ~f:(fun (stack, env) line ->
      match handle stack env line with
      | None -> Stop None
      | Some (s, e) -> Continue (s, e))
    ~finish:(fun (s, _) -> Some (List.rev s))
