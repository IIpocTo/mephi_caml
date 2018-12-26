let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list tail;;

let rec invert l = 
  let rec reverse acc = function
    | [] -> acc
    | head :: tail -> reverse (head :: acc) tail
  in reverse [] l;;

print_string("Task 3.3");;
print_char '\n';;
print_list(invert []);;
print_char '\n';;
print_list(invert [1;2;3;5;7]);;
print_char '\n';;
