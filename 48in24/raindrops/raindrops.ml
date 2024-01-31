let raindrop n =
  let ll = [(n mod 3 , "Pling"); (n mod 5 , "Plang"); (n mod 7 , "Plong")]in
  let rec aa l = match l with
    | x::xs -> if fst x = 0 then (snd x) ^ (aa xs) else aa xs
    | [] -> "" in
  let vv = aa ll in
  if vv = "" then Printf.sprintf "%d" n else vv;;
