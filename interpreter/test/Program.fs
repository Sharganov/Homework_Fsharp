module Program
open Interpretator
open NUnit.Framework
open StParser
open ExeParser
open System.IO
[<TestCase ([|3.0|], ";\nread\np\nwrite\np",Result = [|3.0|]) >]
[<TestCase ([|3.0|], "if\n3\nwrite\n+\n4\n1\nwrite\n5", Result = [|5.0|] )>]
[<TestCase ([|0.0|],  ";\n:=\nres\n4\nwrite\nres", Result = [|4.0|])>]
[<TestCase ([|4.0|],";\nread\nk\n;\n:=\nres\n1\n;\nwhile\nk\n;\n:=\nres\n+\nres\n1\n:=\nk\n-\nk\n1\nwrite\nres",Result = [|5.0|])>]
[<TestCase ([|2.0;3.0|],";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres",Result = [|8.0|])>] 
let ``interpr test`` value str = 
  let data = List.ofArray value
  runtest (stparser str) data

[<TestCase ("+\n3\n2", Result = 5)>]
[<TestCase ("-\n-\n4\n5\n1\n", Result = -2)>]
[<TestCase ("*\n-\n4\n5\n1\n", Result = -1)>]
[<TestCase ("/\n-\n1\n5\n2\n", Result = -2)>]
[<TestCase ("^\n2\n^\n3\n1", Result = 8)>]
let ``calc test`` (str:string )= 
  let list = List.ofArray (str.Split())
  calc (fst (exeparser list)) []
