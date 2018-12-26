let rec length l = match l with
  | [] -> 0
  | head :: tail -> 1 + length tail;;

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

let rec append l1 l2 = match l1 with
  | [] -> l2
  | head :: tail -> head :: append tail l2;;

let flatMap f l =
  reduce (fun p c -> append p (f c)) [] l;;

print_string("Task 3.1");;
print_char '\n';;
print_int(length []);;
print_char '\n';;
print_int(length [1]);;
print_char '\n';;
print_int(length [3;4;5]);;
print_char '\n';;
print_int(length [[];[]]);;
print_char '\n';;
print_int(length [[2;4];[5;6;7;3];[];[2;3;4;5]]);;
print_char '\n';;
