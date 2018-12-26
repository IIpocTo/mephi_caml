let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_string head ; print_string " " ; print_list tail ;;

let rec print_list_int = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list_int tail ;;

let print_bool b = match b with
  | true -> print_string "true"
  | false -> print_string "false";;

let rec member eq elem list = match list with
  | [] -> false
  | (head::tail) -> eq (head, elem) || member eq elem tail;;

let rec rem_from_list eq elem list = match list with
  | [] -> []
  | (head::tail) -> let rest = rem_from_list eq elem tail in
                      if eq(head, elem) 
                      then rest
                      else head::rest;;

let rec make_set eq list = match list with
  | [] -> []
  | (head::tail) -> head :: make_set eq (rem_from_list eq head tail);;

let rec rem_from_set eq elem list = match list with
  | [] -> []
  | (head::tail) -> if eq (head, elem)
                    then tail
                    else head :: rem_from_set eq elem tail;;

let add_to_set eq elem list =
  if member eq elem list then list else elem :: list;;

let rec list_it func list empty = match list with
  | [] -> empty
  | head :: tail -> func head (list_it func tail empty);;

let partition test list =
  let switch elem (l1, l2) =
    if test elem
    then (l1, elem::l2)
    else (elem::l1,l2)
  in list_it switch list ([], []);;

let filter test list = 
  snd (partition test list);;

let inter eq (l1, l2) =
  filter (fun x -> member eq x l2) l1;;

let union eq (l1, l2) =
  list_it (add_to_set eq) l1 l2;;

print_string("2.1");;
print_char '\n';;
print_bool(member (fun(x,y) -> x = y) "bb" ["aa";"bb";"cc"]);;
print_char '\n';;
print_bool(member (fun(x,y) -> x = y) "dd" ["aa";"bb";"cc"]);;
print_char '\n';;

print_char '\n';;
print_string("2.2");;
print_char '\n';;
print_list(rem_from_list (fun(x,y) -> x = y) "bb" ["aa";"bb";"cc";"bb"]);;
print_char '\n';;
print_list(rem_from_list (fun(x,y) -> x = y) "dd" ["aa";"bb";"cc";"bb"]);;
print_char '\n';;

print_char '\n';;
print_string("2.3");;
print_char '\n';;
print_list(make_set (fun(x,y)->x=y) ["aa";"bb";"cc";"bb";"aa";"bb";"cc";"dd"]);;
print_char '\n';;

print_char '\n';;
print_string("2.4");;
print_char '\n';;
print_list(rem_from_set (fun(x,y) -> x = y) "bb" ["aa";"bb";"cc";"bb"]);;
print_char '\n';;
print_list(rem_from_set (fun(x,y) -> x = y) "bb" ["aa";"bb";"cc"]);;
print_char '\n';;

print_char '\n';;
print_string("2.6");;
print_char '\n';;
print_list(add_to_set (fun(x,y) -> x = y) "bb" ["aa";"bb";"cc"]);;
print_char '\n';;
print_list(add_to_set (fun(x,y) -> x = y) "bb" ["aa";"cc"]);;
print_char '\n';;

print_char '\n';;
print_string("2.7");;
print_char '\n';;
print_list_int (filter (fun x -> (x mod 2) = 0) [2;3;5;8;9;12;15]);;
print_char '\n';;
print_list(inter (fun (x,y)->x=y) (["aa";"bb";"cc";"dd"], ["bb";"cc"]));; 
print_char '\n';;

print_char '\n';;
print_string("2.8");;
print_char '\n';;
print_list(union (fun (x,y)->x=y) (["aa";"bb"], ["aa";"cc"]));;
print_char '\n';;
