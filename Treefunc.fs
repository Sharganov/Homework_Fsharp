//Made by Sharganov Atrem
//Ожидаемое время: 3 часа
//Итоговое время : 4 - 5 часов

type BinaryTree<'A> =  
  |Empty
  |Node of 'A * BinaryTree<'A> * BinaryTree<'A>

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
    |res when res > 0 -> Node (c, l ,delete number r)
    |res when res < 0 -> Node (c, delete number l, r)
    |_ ->
      match l, r with
      |Empty, tree -> tree
      |tree, Empty -> tree
      |l, Node(c',Empty, r')->Node(c', l, r')
      |l, Node(c', l', r')-> 
        let n = lastl l'
        Node(n, l, delete n (Node(c', l', r')))

let rec printLCR tree = 
  match tree with
  |Empty -> ()
  |Node(c, l, r) -> 
    printLCR l
    printf "%A" c
    printLCR r
 
let rec printLRC tree = 
  match tree with
  |Empty -> printf ""
  |Node(c, l, r) -> 
    printLRC l
    printLRC r
    printf "%A " c
  
let rec printCLR tree = 
  match tree with
  |Empty -> printf ""
  |Node(c, l, r) -> 
    printf "%A" c
    printCLR l
    printCLR r

let rec Treemap f t =
  match t with
  | Empty -> Empty
  | Node(c, l, r) -> Node (f c, Treemap f l, Treemap f r)

let rec fold f acc t =
  match t with 
  |Empty -> acc      
  |Node (c, l, r) -> 
  fold f (fold f (f acc c) l) r               
                    
let sumtree tree = fold (+) 0 tree  

let min opt x = 
  match opt with
  |None -> Some x   
  |Some y -> Some (min y x)  

let minTree acc x = fold min None x

let rec insert number tree = 
  match tree with 
  | Empty -> Node(number, Empty, Empty)
  | Node(c, l, r) -> 
    match compare number c with
    | res when res < 0 -> Node (c, insert number l, r)
    | res when res > 0 -> Node (c, l, insert number r)
    |_ -> tree
 
let copy tree = 
  fold (fun x y -> insert y x) Empty tree

[<EntryPoint>]
let main argv =
  let tree = Node(2, Node(7, Node(5, Empty, Empty), Empty), Empty)
  printfn "Tree: %A\n" tree
  printfn "map (fun x -> x+5) = tree %A\n)" (Treemap (fun x -> x+5) tree)
  printfn "sum tree = %A\n" (sumtree tree)
  printfn "min tree = %A\n" (minTree None tree)
  let tree1 = copy tree
  printfn "copy tree = %A " tree1
  0  
