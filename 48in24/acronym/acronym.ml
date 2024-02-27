let rec split chars strs =
  match chars with
  | c::cs -> let new_strs = strs |> List.map (String.split_on_char c) |> List.flatten in
             split cs new_strs
  |[] -> strs

let acronym s =
  split [' ';'-'] [s]
  |> List.map (fun s -> (Char.uppercase_ascii (String.get s 0)))
  |> List.to_seq
  |> String.of_seq
