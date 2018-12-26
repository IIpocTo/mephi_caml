type complex = {
  	re : float;
  	im : float;
}

type quad =
  | RealRoots of float * float
  | ComplexRoots of complex * complex ;;

let print_complex (c: complex) = 
  print_float(c.re);
  print_string("+(");
  print_float(c.im);
  print_string("i)");
  ;;

let print_quad q = match q with
  | RealRoots(r1, r2) -> print_float(r1); print_string(", "); print_float(r2)
  | ComplexRoots(c1, c2) -> print_complex(c1); print_string(", "); print_complex(c2);;

let quadsolve a b c =
  let (d: float) = (b *. b) -. (4.0 *. a *. c) in
    if d < 0.0
    then
      let r = -. b /. (2.0 *. a) and i = (sqrt(-. d): float) /. (2.0 *. a) in
        ComplexRoots ({ re = r; im = i }, { re = r; im = (-.i) })
    else
      let r =
        if b < 0.0
        then ((sqrt d) -. b) /. (2.0 *. a)
        else ((sqrt d) +. b) /. (-2.0 *. a) in
          RealRoots (r, c /. (r *. a))
;;

print_string("LAB №2 - Quadratic equation roots");;
print_char '\n';;
print_quad(quadsolve 1.0 0.0 (-2.0));;
print_char '\n';;
print_quad(quadsolve 1.0 0.0 2.0);;
print_char '\n';;
print_quad(quadsolve 1.0 (-1.0e5) 1.0);;
print_char '\n';;
