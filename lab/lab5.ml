type component = Component of int * (int -> int) * (int list);;
type node = Node of component * node list;;
type tree = Tree of node list;;

let components =[ 
  Component(1, (fun x -> x), [3;4]);
  Component(2, (fun x -> x), [4;5;6]);
  Component(3, (fun x -> x), []); 
  Component(4, (fun x -> x), [5;8;9]); 
  Component(5, (fun x -> x), [6;7;8]); 
  Component(6, (fun x -> x), [7;9]);
  Component(7, (fun x -> x), [8]); 
  Component(8, (fun x -> x), [9]); 
  Component(9, (fun x -> x), [])
];;

let rec indexof el l = match l with 
  | [] -> -1
  | a::tl -> if a = el 
             then 0 
             else (1 + (indexof el tl));;

let rec map f l = match l with
  | [] -> []
  | head :: tail -> f(head) :: map f tail;;

let rec inserti = fun comp node -> let (Component(i,_,_)) = comp in 
  match node with 
  | Node (Component(_,_,[]),_) -> node
  | Node (Component(id,f,l),children) -> let newChildren =
      map (inserti comp) children in
        let newChildren2 = (
          if indexof i l >= 0
          then Node(comp,[]) :: newChildren
          else newChildren) in 
            Node (Component(id,f,l), newChildren2);;
            
let insertTree = fun tree c -> match tree with
  | Tree [] -> Tree [(Node (c, []))]
  | Tree nodes -> Tree (map (inserti c) nodes);;
  
let rec reduce = fun f seed lst -> match lst with 
  | [] -> seed
  | head::tail -> reduce f (f seed head) tail;;
  
let tree = Tree [];;
let resultTree = reduce insertTree tree components;;
