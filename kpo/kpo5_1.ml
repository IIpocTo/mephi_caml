let rec find prop list = match list with
  | [] -> failwith "no such item"
  | (head::tail) -> if prop head
                    then head
                    else find prop tail;;

print_string("5.1");;
print_char '\n';;
print_string(find (fun x->x = "Peter") ["Nick";"Jim";"Peter"]);;
print_char '\n';;
print_string(find (fun x->x = "Peter") ["Nick";"Jim"]);;
print_char '\n';;
