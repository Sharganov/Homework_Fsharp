//Made by Sharganov Atrem
//Ожидаемое время: 3 часа
//Итоговое время : 4 - 5 часов

type BinaryTree<'A> =  
  |Empty
  |Node of ('A) * BinaryTree<'A> * BinaryTree<'A>

type Option<'A> = None | Some of 'A

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
