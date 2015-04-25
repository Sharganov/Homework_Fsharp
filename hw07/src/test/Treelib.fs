module Treelib

type BinaryTree<'A> = 
 | Empty
 | Node of 'A*BinaryTree<'A>*BinaryTree<'A>

let rec Treemap f t =
  match t with
  | Empty -> Empty
  | Node(c, l, r) ->
    Node(f c, Treemap f l, Treemap f r)


let rec insert number tree = 
  match tree with 
  | Empty -> Node(number, Empty, Empty)
  | Node(c, l, r) -> 
    match compare number c with
    | res when res < 0 -> Node (c, insert number l, r)
    | res when res > 0 -> Node (c, l, insert number r)
    |_ -> tree


type acc(tree:BinaryTree<int>) = 
  let mutable tree = tree
  member s.push x = tree <- insert x tree
  member s.get = tree

let rec merge (t1) (t2):BinaryTree<'A> =
  let res = new acc(t1)
  let rec In (t2:BinaryTree<'A>) =  
    match t2 with 
    | Empty-> Empty
    | Node(x,l,r) -> 
       res.push x
       ignore(In l)
       In r
  In t2
  res.get

let root t = 
  match t with 
  | Node(x, Empty, Empty) -> 
      x
      
let rec concat t =
  let res = new acc(Empty)
  let  rec ins t = 
      match t with
      | Node(Empty, x, y) -> 
          ins x
          ins y
      | Node(x,y,z) ->
         res.push (root x) 
         ins y 
         ins z
      | Empty -> ()
  ins t
  res.get

