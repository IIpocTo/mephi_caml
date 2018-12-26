let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list tail;;

let rec map f l = match l with
  | [] -> []
  | head :: tail -> f(head) :: map f tail;;

print_string("Task 3.4");;
print_char '\n';;
print_list(map (fun x -> x*x) []);;
print_char '\n';;
print_list(map (fun x -> x*x) [1;3;5]);;
print_char '\n';;
