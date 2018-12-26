print_string("5.3");;
print_char '\n';;
let sample = open_out "try1.txt" in
  output_string sample "hey, buddy";
  close_out sample;;
print_string "the string has been successfully written into the file";;
print_char '\n';;
