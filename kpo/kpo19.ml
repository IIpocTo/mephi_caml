let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list tail;;

let rec length l = match l with
  | [] -> 0
  | head :: tail -> 1 + length tail;;

print_string("19.1");;
print_char '\n';;
print_int(length [3;4;5]);;
print_char '\n';;

let rec append l1 l2 = match l1 with
  | [] -> l2
  | head :: tail -> head :: append tail l2;;

print_char '\n';;
print_string("19.2");;
print_char '\n';;
print_list (append [3;4;5] [6;7]);;
print_char '\n';;

let rec rev l = match l with
  | [] -> []
  | head :: tail -> append (rev tail) [head];;

print_char '\n';;
print_string("19.3");;
print_char '\n';;
print_list(rev [3;4;5]);;
print_char '\n';;

let rec sigma l = match l with
  | [] -> 0
  | head :: tail -> head + sigma tail;;

print_char '\n';;
print_string("19.4");;
print_char '\n';;
print_int(sigma [3;4;5]);;
print_char '\n';;

let rec pi l = match l with
  | [] -> 1
  | head :: tail -> head * pi tail;;

print_char '\n';;
print_string("19.5");;
print_char '\n';;
print_int(pi [3;4;5]);;
print_char '\n';;

let rec map f l = match l with
  | [] -> []
  | head :: tail -> f(head) :: map f tail;;

print_char '\n';;
print_string("19.6");;
print_char '\n';;
print_list(map (fun x -> x + 3) (3::4::[5]));;
print_char '\n';;
print_list(map (fun x -> x + 3) [3;4;5]);;
print_char '\n';;

let rec flat l = match l with
  | [] -> []
  | head :: tail -> append head (flat tail);;

print_char '\n';;
print_string("19.7");;
print_char '\n';;
print_list(flat [[3;4];[5]]);;
print_char '\n';;

let rec list_hom empty func list = match list with
  | [] -> empty
  | head :: tail -> func head (list_hom empty func tail);;

let cons = fun elem list -> elem :: list;;

print_char '\n';;
print_string("19.8");;
print_char '\n';;

let length_hom = list_hom 0 (fun h t -> t + 1);;

print_int(length_hom [1;2;3;4]);;
print_char '\n';;

let append_hom l1 l2 = list_hom l2 cons l1;;

print_list (append_hom [3;4;5] [6;7]);;
print_char '\n';;

let rev_hom l = list_hom [] (fun elem list -> append list [elem]) l;;

print_list(rev_hom [3;4;5]);;
print_char '\n';;

let rec sigma_hom = list_hom 0 (fun elem add -> elem + add);;

print_int(sigma_hom [3;4;5]);;
print_char '\n';;

let rec pi_hom = list_hom 1 (fun elem mult -> elem * mult);;

print_int(pi_hom [3;4;5]);;
print_char '\n';;

let rec map_hom f l = list_hom [] (fun elem list -> f(elem) :: list) l;;

print_list(map_hom (fun x -> x + 3) (3::4::[5]));;
print_char '\n';;
print_list(map_hom (fun x -> x + 3) [3;4;5]);;
print_char '\n';;

let rec flat_hom = list_hom [] append;;

print_list(flat_hom [[3;4];[5]]);;
print_char '\n';;

let rec list_it func list empty = match list with
  | [] -> empty
  | head :: tail -> func head (list_it func tail empty);;

print_char '\n';;
print_string("19.10");;
print_char '\n';;
print_int(list_it (fun x y -> x + y) [1;2;3;4] 0);;
print_char '\n';;
print_int(list_it (fun x y -> x * y) [1;2;3;4] 1);;
print_char '\n';;

let rec it_list func empty list = match list with
  | [] -> empty
  | head :: tail -> it_list func (func empty head) tail;;

print_char '\n';;
print_string("19.11");;
print_char '\n';;
print_int(it_list (fun x y -> x + y) 0 [1;2;3;4]);;
print_char '\n';;
print_int(it_list (fun x y -> x * y) 1 [1;2;3;4]);;
print_char '\n';;
