let response_for s =
  let open String in
  let open Base.Char in
  let s' = String.trim s in
  if for_all is_whitespace s' then "Fine. Be that way!"
  else
    match
      ( ends_with ~suffix:"?" s',
        (String.equal s' @@ uppercase_ascii s') && exists is_alpha s' )
    with
    | true, true -> "Calm down, I know what I'm doing!"
    | true, false -> "Sure."
    | false, true -> "Whoa, chill out!"
    | false, false -> "Whatever."
