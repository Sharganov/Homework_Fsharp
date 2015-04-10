module Program
open Interpretator
open NUnit.Framework
open StParser
open ExeParser

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

[<EntryPoint>]
let main argv = 
  let fk = ";\nread\nk\n;\nread\nm\n;\nread\nl\n;\nread\np\nwrite\nk"
  let str1 = "if\n3\nwrite\n+\n4\n1\nwrite\n5"
  let str =  ";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres"
  let str3  = ";\nread\x\n;\nread\nn\n;\nif\nx\n;\nwrite\n+\nx\n1\nwrite\n*\nn\n2\nwrite\nx"
  let sfg = ";\nread\nx\n;\nwrite\nx\nread\np\n;\nread\nt"
  let please = "if\nx\n;\nwrite\n+\nx\n1\nwrite\n*\nn\n2\nwrite\nx"
  let df = ";\nread\nk\n;\n:=\nres\n1\n;\nwhile\nk\n;\n:=\nres\n+\nres\n1\n:=\nk\n-\nk\n1\nwrite\nres"
  let str = "*\nres\nx"
  printfn "%A" (exeparser (List.ofArray (str.Split())))
 // printfn "%A" (stparser df)
 // let res = runtest (stparser df) [3.0;4.0;5.0;3.0]
 // printf "%A" res
  //run (stparser str)
  0
