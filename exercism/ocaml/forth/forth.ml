open Base

type word = Num of int | Str of string
type forth = { stack : int list; env : word list Hashtbl.M(String).t }

let word_of_string s = try Num (Int.of_string s) with _ -> Str s
let def name words forth = Hashtbl.set forth.env ~key:name ~data:words

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

let app forth = function
  | Num i -> Some { forth with stack = i :: forth.stack }
  | Str s when Hashtbl.mem forth.env s -> failwith "todo"
  | Str s ->
      builtin s forth.stack |> Option.map ~f:(fun stack -> { forth with stack })

let evaluate = failwith "todo"
