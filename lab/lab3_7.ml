let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list tail;;

let rec reduce f zero l = match l with
  | [] -> zero
  | head :: tail -> reduce f (f zero head) tail;;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | head :: tail -> head :: append tail l2;;

let flatMap f l =
  reduce (fun p c -> append p (f c)) [] l;;

print_string("Task 3.7");;
print_char '\n';;
print_list(flatMap (fun x -> [x*x]) []);;
print_char '\n';;
print_list(flatMap (fun x -> [x*x]) [1;3;5]);;
print_char '\n';;
