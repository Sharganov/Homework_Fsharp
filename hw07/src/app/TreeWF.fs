module TreeWF
open Treelib

type TreeBuilder () = 
  member s.Return x = x
  member s.ReturnFrom x = printfn "%A" x 
  member s.Bind (x,f) = 
    x |> Treemap f|>concat
  member s.Combine (a, b) = 
    merge a b
  member s.Delay f = f()
  member s.For (x,f) = 
    s.Bind(x,f)
 
let treeWorkFlow = new TreeBuilder ()

let map p (t:BinaryTree<'A>) = 
  treeWorkFlow {
    for x in t do 
      return (Node(p x,Empty,Empty))
  }
    
let filter (p:'A->bool) (t:BinaryTree<'A>) =
  treeWorkFlow {
    for x in t do
      return (if p x then Node (x, Empty, Empty) else Empty)
}

let  treecalc oper t1 t2 =
  treeWorkFlow {
    for x in t1 do
    for y in t2 do 
    return Node( oper x y, Empty, Empty)
  } 