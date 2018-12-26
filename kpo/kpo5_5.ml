let rec y_comb f x = f (y_comb f) x;;
let fac f = function
  | 0 -> 1
  | n -> n * f (n-1);;

print_string("5.5");;
print_char '\n';;
print_int(y_comb fac 6);;
print_char '\n';;
