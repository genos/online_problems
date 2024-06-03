let reverse_string = Base.String.rev

(* let reverse_string = String.fold_left (fun a c -> String.make 1 c ^ a) "" *)

(* let reverse_string s = *)
(*   let n = String.length s in *)
(*   String.mapi (fun i _ -> s.[n - i - 1]) s *)

(* let reverse_string s = *)
(*   let out = ref "" in *)
(*   String.iter (fun c -> out := String.make 1 c ^ !out) s; *)
(*   !out *)
