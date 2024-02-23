

let length l = 
  List.length l

let rec reverse l =
  match l with
  | [] -> []
  | a::b::[] -> b :: a :: []
  | a::xs -> reverse xs @ [a]

let rec map ~f ll =
  match ll with
  | x :: xs -> f x :: (map ~f xs)
  | [] -> []

let rec filter ~f ll =
  match ll with
  | x ::xs -> if f x then x :: filter ~f xs else filter ~f xs
  | [] -> []

let rec fold ~init ~f ll =
  match ll with
  | x:: xs -> fold ~init:(f init x) ~f xs
  | [] -> init

let append a b =
  a@b

let concat ll =
  match ll with 
  | x::xs -> fold ~init:x ~f:(@) xs
  | [] -> []

