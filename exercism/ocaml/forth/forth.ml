open Base

type word = Num of int | Str of string
type forth = { stack : int list; env : word list Hashtbl.M(String).t }

let word_of_string s = try Num (Int.of_string s) with _ -> Str (String.lowercase s)

let def name words forth =
  Hashtbl.set forth.env ~key:name ~data:words;
  Some forth

let is_builtin =
  Set.mem (Set.of_list (module String) [ "+"; "-"; "*"; "/"; "dup"; "drop"; "swap"; "over" ])

let builtin = function
  | "+"    -> (function [] -> None | [_] -> None | x :: y :: zs -> Some (y + x :: zs))
  | "-"    -> (function [] -> None | [_] -> None | x :: y :: zs -> Some (y - x :: zs))
  | "*"    -> (function [] -> None | [_] -> None | x :: y :: zs -> Some (y * x :: zs))
  | "/"    -> (function [] -> None | [_] -> None | 0 :: _ -> None | x :: y :: zs -> Some (y / x :: zs))
  | "dup"  -> (function [] -> None | x :: xs -> Some (x :: x :: xs))
  | "drop" -> (function [] -> None | _ :: xs -> Some xs)
  | "swap" -> (function [] -> None | [_] -> None | x :: y :: zs -> Some (y :: x :: zs))
  | "over" -> (function [] -> None | [_] -> None | x :: y :: zs -> Some (y :: x :: y :: zs))
  | _      -> Fn.const None  [@@ocamlformat "disable"]

let rec lookup forth = function
  | Num n -> [ Num n ]
  | Str name -> (
      match (Hashtbl.find forth.env name, is_builtin name) with
      | None, false -> []
      | None, true -> [ Str name ]
      | Some words, _ -> List.concat_map words ~f:(lookup forth))

let rec app forth = function
  | Num i -> Some { forth with stack = i :: forth.stack }
  | Str s ->
      Option.first_some
        (Hashtbl.find forth.env s |> Option.map ~f:(app_all forth) |> Option.join)
        (builtin s forth.stack |> Option.map ~f:(fun stack -> { forth with stack }))

and app_all forth =
  List.fold_until ~init:forth
    ~f:(fun f w -> match app f w with None -> Stop None | Some f' -> Continue f')
    ~finish:Option.some

let words_of_line line =
  String.split ~on:' ' line
  |> List.filter_map ~f:(fun s -> if String.is_empty s then None else Some (word_of_string s))

let handle forth line =
  if String.is_prefix ~prefix:":" line && String.is_suffix ~suffix:";" line then
    match words_of_line (String.strip ~drop:(fun c -> Char.(equal c ':' || equal c ';')) line) with
    | [] | [ _ ] | Num _ :: _ -> None
    | Str name :: rest -> (
        match List.concat_map rest ~f:(lookup forth) with
        | [] -> None
        | words -> def name words forth)
  else words_of_line line |> app_all forth

let evaluate lines =
  List.fold_until lines
    ~init:{ stack = []; env = Hashtbl.create (module String) }
    ~f:(fun f line -> match handle f line with None -> Stop None | Some f' -> Continue f')
    ~finish:Option.some
  |> Option.map ~f:(fun forth -> List.rev forth.stack)
