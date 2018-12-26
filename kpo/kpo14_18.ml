let print_pair_int pair =
  print_string("(");
  print_int(fst pair);
  print_string(", ");
  print_int(snd pair);
  print_string(")");;

let rec print_list_int = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list_int tail;;

type ('a) lnode = {info: 'a; mutable next: 'a lnode}

let rec print_lnode x depth = 
  if (depth < 10)
  then (print_string("{"); print_string(x.info); print_lnode(x.next)(depth + 1); print_string("}"))
  else ();;

let mk_circular_list e =
  let rec x = {info = e; next = x} in x;;

let rec list_it func list empty = match list with
  | [] -> empty
  | head :: tail -> func head (list_it func tail empty);;

let insert_tail e l = 
  let x = { info = e; next = l.next } in 
    l.next <- x; x;;

let circular =
  let ll = mk_circular_list "Combinatory" in
    list_it insert_tail ["Book:";"programming";"in";"logic"] ll;;

type ('b) queue = Emptyqueue | Queue of 'b lnode;;

let print_queue q = match q with
  | Queue (ln) -> print_lnode(ln)(0)
  | Emptyqueue -> ();;

let enqueue x q = match q with
  | Emptyqueue -> Queue (mk_circular_list x)
  | Queue (ln) -> Queue (insert_tail x ln);;

let list_of_queue = fun q -> match q with 
  | Emptyqueue -> []
  | (Queue ln) -> let ln1 = ln.next in 
                    let rec loq ln = 
                      if ln == ln1 
                      then [] 
                      else ln.info :: loq ln.next in 
                        ln1.info :: loq ln1.next;;

type ('c) dblnode = {info: 'c; mutable prev: 'c dblnode; mutable next: 'c dblnode};; 

let rec print_dblnode x depth = 
  if (depth < 10)
  then (print_string("{"); print_string(x.info); print_dblnode(x.prev)(depth + 1); print_string("}"))
  else ();;

let mk_dbl_circular_list e =
  let rec x = {info = e; prev = x; next = x} in x;;

let elim l =
  let lprev = l.prev and lnext = l.next in
    lprev.next <- lnext;
    lnext.prev <- lprev; 
    lprev;;

let insert_after e l = 
  let lnext = l.next in 
    let x = { info = e; prev = l; next = lnext } in 
      lnext.prev <- x;
      l.next <- x;;

let sym c = ("book"^(string_of_int c), c + 1);;

let add_int c m n = (m + n, c);;

let comp c f g x =
  let (v1, c1) = g c x 
    in f c1 v1;;

let add3 c x = (x + 3, c);;
let add4 c x = (x + 4, c);; 

let update mem loc v = 
  fun loc1 ->
    if loc1 = loc
    then v
    else mem loc;;

print_string("14");;
print_char '\n';;
print_lnode(circular)(0);;
print_char '\n';;

print_char '\n';;
print_string("15");;
print_char '\n';;

let q1 = enqueue "Book:"
  (enqueue "programming"
  (enqueue "in"
  (enqueue "logic"
  (enqueue "Combinatory" Emptyqueue))));;

print_queue(q1);;
print_char '\n';;

print_char '\n';;
print_string("16");;
print_char '\n';;

let dcl = mk_dbl_circular_list "Book:";;
insert_after "Combinatory" dcl;;
insert_after "logic" dcl;;
insert_after "in" dcl;;
insert_after "programming" dcl;;

print_dblnode(dcl)(0);;
print_char '\n';;

elim dcl;;
print_dblnode(dcl)(0);;
print_char '\n';;

print_char '\n';;
print_string("17");;
print_char '\n';;
print_pair_int(comp 0 add4 add3 1);;
print_char '\n';;

print_char '\n';;
print_string("18");;
print_char '\n';;
print_list_int(update(fun x -> [x])(15)([3])(0));;
print_char '\n';;
