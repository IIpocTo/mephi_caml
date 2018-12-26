open List

let hd list = match list with
  | [] -> raise(Failure "hd")
  | (0 :: tail) -> raise(Failure "zero")
  | (head :: tail) -> head;;

print_string("7.1");;
print_char '\n';;

print_int(try (hd []) with Failure "hd" -> 7);;
print_char '\n';;

print_int(hd [-1; 0; 1]);;
print_char '\n';;
print_int(hd (tl [-1; 0; 1]));;
print_char '\n';;
print_int(hd (tl (tl [-1; 0; 1])));;
print_char '\n';;
print_int(hd (tl(tl (tl [-1 ; 0 ; 1]))));;
print_char '\n';;
