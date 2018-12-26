let rec get index l = match l with
  | [] -> raise (Failure "index out of range")
  | head :: tail -> if index = 0 then head else get (index - 1) tail;;

print_string("Task 3.2");;
print_char '\n';;
print_int(get 2 [0;1;2;3;4]);;
print_char '\n';;
print_int(get 2 []);;
print_char '\n';;
print_int(get 0 []);;
print_char '\n';;
print_int(get 2 [0;1]);;
print_char '\n';;