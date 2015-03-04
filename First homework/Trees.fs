//By Sharganov Artem
//Предполагаемое время написания: 3-4 часа (1 вечер)
//Итоговое время:3 часа(вместе с чтнеием литературы)

type BinaryTree =
  | Empty
  | Node of  int*BinaryTree *  BinaryTree


let rec insert number tree = 
  match tree with 
  | Empty -> Node(number, Empty, Empty)
  | Node(c, l, r) -> 
    match compare number c with
    | res when res < 0 -> Node (c, insert number l, r)
    | res when res > 0 -> Node (c, l, insert number r)
    |_ -> tree

let rec lastl tree = 
  match tree with 
    |Empty-> 0
    |Node(c, Empty, _)-> c
    |Node(c, l, _)-> lastl l

let rec delete number tree = 
  match number, tree with
  |_, Empty -> Empty
  |number, Node(c, l, r) ->
    match compare number c with
    |res > 0 -> Node (c, l ,delete number r)
    |res < 0 -> Node (c, delete number l, r)
    |_ ->
      match l, r with
      |Empty, _ -> _
      |_, Empty -> _
      |l, Node(c',Empty, r')->Node(c', l, r')
      |l, Node(c', l', r')-> Node(lastl l', l, delete (lastl l') (Node(c', l', r')))

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
  printf "Tree: %A\n" tree
  let tree1 = insert -1 tree
  printf "Insert -1: %A\n" tree1
  let tree2 = insert 5 tree1
  printf "Insert 5: %A\n" tree2
  let tree3 = delete 2 tree2
  printf "Delete 2: %A\n" tree3
  let tree4 = insert 17 tree3
  printf "Insert 17: %A\n" tree4
  printf "printCLR "
  printCLR tree4
  printf "\nprintLCR "
  printLCR tree4
  printf "\nprintLRC "
  printLRC tree4
  0
  
 
