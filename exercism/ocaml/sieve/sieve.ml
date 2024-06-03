let primes n =
  let open Base in
  let bits = Array.create ~len:(n + 1) true in
  bits.(0) <- false;
  bits.(1) <- false;
  for i = 2 to n do
    if bits.(i) then
      let j = ref (i + i) in
      while !j <= n do
        bits.(!j) <- false;
        j := !j + i
      done
  done;
  Array.to_list @@ Array.filter_mapi ~f:(fun i b -> Option.some_if b i) bits
