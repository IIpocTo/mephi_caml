let rec facti m t = if m = 0 then t else facti (m - 1) (t * m);;
let fac m = facti m 1;;

print_string("6.2");;
print_char '\n';;
print_int(fac 6);;
print_char '\n';;