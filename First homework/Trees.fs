type BinaryTree =
  | Empty
  | Node of  int*BinaryTree *  BinaryTree


let rec insert number tree = 
  match number, tree with 
  |number, Empty -> Node(number, Empty, Empty)
  |number, Node(c, l, r) -> 
    if number = c then  Node(number, l, r)
    else if number < c then Node(c, insert number l, r)
    else Node(c, l, insert number r)

let rec lastleft tree = 
  match tree with 
    |Empty-> 0
    |Node(c, Empty, _)-> c
    |Node(c, l, _)-> lastleft l

let rec delete number tree = 
  match number, tree with
  |_, Empty -> Empty
  |number, Node(c, l, r) -> 
    if number > c then Node(c, l, delete number r)
    else if number < c then Node(c, delete number l, r)
    else 
      match l, r with
      |Empty, Empty -> Empty
      |Empty, Node(c'', l'', r'')->Node(c'', l'', r'')
      |Node(c', l', r'), Empty -> Node(c', l', r')
      |l, Node(c'',Empty, r'')->Node(c'', l, r'')
      |l, Node(c'', l'', r'')-> Node(lastleft l'', l, delete (lastleft l'') (Node(c'', l'', r'')))

let rec printLCR tree = 
  match tree with
  |Empty -> ()
  |Node(c, l, r) -> 
    printLCR l
    printf "%d " c
    printLCR r
 

let rec printLRC tree = 
  match tree with
  |Empty -> printf ""
  |Node(c, l, r) -> 
    printLRC l
    printLRC r
    printf "%d " c
  

let rec printCLR tree = 
  match tree with
  |Empty -> printf ""
  |Node(c, l, r) -> 
    printf "%d " c
    printCLR l
    printCLR r

[<EntryPoint>]
let main argv =
  let tree = Node(2, Empty, Empty)
  printf "%A" tree
  let tree2 = insert 5 tree
  System.Console.Read();
  