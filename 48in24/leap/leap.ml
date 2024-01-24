let leap_year yrs =
  match (yrs mod 4) with
  | 0 -> if (yrs mod 400) = 0 then 
           true
         else if (yrs mod 100) = 0 then
           false
         else
           true
  | _ -> false
  ;;
