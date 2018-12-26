let rec reduce f zero l = match l with
  | [] -> zero
  | head :: tail -> reduce f (f zero head) tail;;

print_string("Task 3.5");;
print_char '\n';;
print_int(reduce (fun p c -> p*c) 1 []);;
print_char '\n';;
print_int(reduce (fun p c -> p*c) 1 [1;3;5;7;9]);;
print_char '\n';;
