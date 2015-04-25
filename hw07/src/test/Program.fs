module Program
open NUnit.Framework
open ArithmeticWF
open TreeWF
open Treelib

let tree = Node (3, Node(1, Empty, Node(2, Empty,Empty)), Node(8, Node(7,Empty, Empty),Empty))

[<Test>]
let ``Treetest map``() = 
  let res = map (fun x -> x+5) tree
  Assert.AreEqual (res,Node (8, Node(6, Empty, Node(7, Empty,Empty)), Node(13, Node(12,Empty, Empty),Empty)) )

[<Test>]
let ``Treetest filter`` () =
  let res = filter (fun x -> x=1) tree
  Assert.AreEqual(res, Node(1,Empty, Empty))

[<Test>]
let ``sum trees`` () = 
  let t1 = Node(3, Empty, Empty)
  let t2 = Node(4, Empty, Empty)
  let res = treecalc (+) t1 t2
  Assert.AreEqual (res, Node(7, Empty, Empty))


let ring x = new RingBuilder (x)

[<TestCase (2,3, 4, Result = 1)>]
[<TestCase (6, 2, 5, Result = 1)>]
let test m x y =
  ring  m {
    return x+y
  } 
[<TestCase (5,7, 9, Result = 3)>]
[<TestCase (2, 7, 4, Result = 1)>]
let test1 m x y =
  ring  m {
    let! a = x
    let! b = y
    return a-b
  } 

[<TestCase (2,3, 4, Result = 0)>]
[<TestCase (6, 2, 5, Result = 4)>]
let test3 m x y =
  ring  m {
    let! a = x
    let! b = y
    return a*b
  } 




