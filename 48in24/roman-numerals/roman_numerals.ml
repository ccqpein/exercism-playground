let to_roman num: string =
  let table = [
        (1, "I");
        (4, "IV");
        (5, "V");
        (9, "IX");
        (10, "X");
        (40, "XL");
        (50, "L");
        (90, "XC");
        (100, "C");
        (400, "CD");
        (500, "D");
        (900, "CM");
        (1000, "M");
    ]
  and orders = [1000; 900; 500; 400; 100; 90; 50; 40; 10; 9; 5; 4; 1]
  and result = ref ""
  in
  let rec inner_loop num s =
    if num = 0 then s
    else if Option.is_some (List.assoc_opt num table) then s ^ List.assoc num table
    else let new_s = ref s in
         let rec loop_order orders num =
             match orders with
             |v ::xs -> if num > v
                        then
                          begin 
                          for time = 0 to num / v -1 do new_s := !new_s ^ List.assoc v table done;
                          inner_loop (num mod v) !new_s
                          end 
                        else loop_order xs num
             |[] -> inner_loop num !new_s in
         loop_order orders num
  in
  inner_loop num !result;;
