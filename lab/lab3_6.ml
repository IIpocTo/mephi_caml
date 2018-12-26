let rec reduce f zero l = match l with
  | [] -> zero
  | head :: tail -> reduce f (f zero head) tail;;

let min l = match l with
  | [] -> raise (Failure "min on empty list")
  | head :: tail -> reduce (fun x1 x2 -> if x1 < x2 then x1 else x2) head tail;;

let max l = match l with
  | [] -> raise (Failure "min on empty list")
  | head :: tail -> reduce (fun x1 x2 -> if x1 > x2 then x1 else x2) head tail;;

let sum l = reduce (fun p c -> p + c) 0 l;;

let avg l = 
  let (sum, length) = reduce (fun (s, l) x -> (x + s, 1 + l)) (0, 0) l
    in float_of_int sum /. float_of_int length;;

print_string("Task 3.6");;
print_char '\n';;
print_int(min [1;3;5;7;9]);;
print_char '\n';;
print_int(max [1;3;5;7;9]);;
print_char '\n';;
print_int(sum [1;3;5;7;9]);;
print_char '\n';;
print_float(avg [1;3;5;7;9]);;
print_char '\n';;
print_int(min []);;
print_char '\n';;
