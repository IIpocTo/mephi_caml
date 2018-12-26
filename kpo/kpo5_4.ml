let in_test f = print_string "First test: ";
  let input1 = float_of_string (read_line()) in print_string "Second test: ";
  let input2 = float_of_string (read_line()) in f(input1, input2);;

print_string("5.4");;
print_char '\n';;
in_test (fun(input1, input2) ->
  let avg = (input1 +. input2) /. 2.0 in 
    print_string (if (avg >= 3.0)
                    then "PASSED ==> "
                    else "DECLINED ==> ");
  print_float avg; print_newline()
);;
