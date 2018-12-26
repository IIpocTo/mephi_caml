let failwith message = raise (Failure message);;

print_string("7.2");;
print_char '\n';;
failwith "Fail with some exception."
