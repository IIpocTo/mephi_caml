let rec print_list = function 
  | [] -> ()
  | head :: tail -> print_string head ; print_string " " ; print_list tail;;

let rec print_list_int = function 
  | [] -> ()
  | head :: tail -> print_int head ; print_string " " ; print_list_int tail;;

let rec print_list_char = function 
  | [] -> ()
  | head :: tail -> print_char head ; print_string " " ; print_list_char tail ;;

let stream_of_char = Stream.of_list(['a';'b';'c']);;
let stream_of_string = Stream.of_list(["aa";"bb";"cc"]);;

let stream_m = 
  let m = 3 in
    Stream.of_list([1+2*m; 2+2*m;3+2*m]);;

let conc_stream = 
  let st1 = Stream.of_list(['1';'2';'3']) in
    let st2 = Stream.of_list(['a';'b';'c']) in
      Stream.of_list('0' :: Stream.npeek 10 st1 @ Stream.npeek 10 st2);;

let infinite_stream =
  let s = 0 in 
    Stream.from (fun i -> Some (i + s));; 

print_string("12");;
print_char '\n';;
print_list_char(Stream.npeek 10 stream_of_char);;
print_char '\n';;
print_list(Stream.npeek 10 stream_of_string);;
print_char '\n';;
print_list_int(Stream.npeek 10 stream_m);;
print_char '\n';;

print_char '\n';;
print_string("12.1");;
print_char '\n';;
print_list_char(Stream.npeek 10 conc_stream);;
print_char '\n';;

print_char '\n';;
print_string("12.2");;
print_char '\n';;

let stream_vs = Stream.from(
  fun n -> match n with
    | 0 -> Some 'a'
    | 1 -> Some (print_string "List vs stream: from stream\n"; 'b')
    | 2 -> Some '3'
    | _ -> None
);;
let list_vs = ['a';(print_string "List vs stream: from list\n"; 'b');'3'];;

print_char '\n';;
print_string("12.3");;
print_char '\n';;
print_list_int(Stream.npeek 33 infinite_stream);;
print_char '\n';;

print_char '\n';;
print_string("12.4");;
print_char '\n';;
print_int(Stream.next infinite_stream);;
print_char '\n';;
print_int(Stream.next infinite_stream);;
print_char '\n';;
print_int(Stream.next infinite_stream);;
print_char '\n';;
print_int(Stream.next infinite_stream);;
print_char '\n';;
