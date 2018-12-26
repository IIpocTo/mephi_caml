let cI x = x;;
let cK x y = x;;
let cS x y z = x z (y z);;
let cB x y z = x (y z);;
let cC x y z = x z y;;

print_int (cI 1);;
print_char '\n';;
print_int (cK 1 2);;
print_char '\n';;
print_int (cS (fun a b -> a + b) (fun a -> a + 1) 2);;
print_char '\n';;
print_int (cB (fun x -> x + 1) (fun x -> x + 3) 4);;
print_char '\n';;
print_int (cC (fun a b -> a / b) 2 4);;
print_char '\n';;

assert (cI 3 = cS cK cK 3);;
assert (cB (fun x -> x + 1) (fun x -> x + 3) 4 = cS (cK cS) cK (fun x -> x + 1) (fun x -> x + 3) 4);;
assert (cC (fun a b -> a / b) 2 4 = cS (cB cB cS) (cK cK) (fun a b -> a / b) 2 4);;
print_char '\n';;

let cW x y = cC cS cI x y;;
let cPsi f g x y = cB (cB cW (cB cC))(cB cB (cB cB)) f g x y;;
let cF f g h x= cB cB cB cS cB f g h x;;

print_int (cW (fun x y -> x + y) 5);;
print_char '\n';;
print_int (cPsi (fun x y -> x + y) (fun x -> x * 2) 2 5);;
print_char '\n';;
print_int (cF (fun x y -> x + y) (fun x -> x * 2) (fun x -> x * 3) 5);;
print_char '\n';;

type contract = {
  sum1: float;
  sum2: float;
  sum3: float option;
  agreed: string option;
  approved: string option;
}

let createContract_1 sum_1 sum_2 = {
  sum1 = sum_1;
  sum2 = sum_2;
  sum3 = None;
  agreed = None;
  approved = None;
};;

let createContract_2 sum1 sum2 sum3 = {
  sum1 = sum1;
  sum2 = sum2;
  sum3 = sum3;
  agreed = None;
  approved = None;
};;

let riskAccess c = let calc s1 s2 = 
  (s1 -. s2) /. (s1 +. s2) +. (s1 +. s2) /. 2.0 in {
    sum1 = c.sum1;
    sum2 = c.sum2;
    sum3 = Some (calc c.sum1 c.sum2);
    agreed = c.agreed;
    approved = c.approved;
  };;

let checkContract_1 c = 
  if (c.sum1 +. c.sum2 +. match c.sum3 with None -> 0.0 | Some x -> x) > 0.0
  then c,true
  else c,false;;

let checkContract_2 c = let res = c.sum1 +. c.sum2 +. match c.sum3 with None -> 0.0 | Some x -> x in
  if res > 0.0 && res < 100.0 
  then c,true 
  else c,false;;

let signContract_1 c = {
  sum1 = c.sum1;
  sum2 = c.sum2;
  sum3 = c.sum3;
  agreed = c.agreed;
  approved = None;
};;

let signContract_1 c = {
  sum1 = c.sum1;
  sum2 = c.sum2;
  sum3 = c.sum3;
  agreed = c.agreed;
  approved = None;
};;

let signContract_1 c = {
  sum1 = c.sum1;
  sum2 = c.sum2;
  sum3 = c.sum3;
  agreed = c.agreed;
  approved = None;
};;

let signContract_2 c = {
  sum1 = c.sum1;
  sum2 = c.sum2;
  sum3 = c.sum3;
  agreed = c.agreed;
  approved = c.approved;
};;

let signContract_3 c = {
  sum1 = c.sum1;
  sum2 = c.sum2;
  sum3 = c.sum3;
  agreed = None;
  approved = c.approved;
};;
