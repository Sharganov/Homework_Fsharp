module Interpretator
open StParser
open ExeParser

type IInput =
  interface 
    abstract Read : Option<float>
  end 

type UserInput = 
  class
    interface IInput with
      member s.Read =
        Some (System.Convert.ToDouble (System.Console.Read()))
  end
type ListInput<'A> (data : float list) = 
  class
    let mutable data = data
    interface IInput with
      member s.Read =   
        if data.IsEmpty
        then None
        else 
          let head = data.Head
          data <- data.Tail
          Some head
  end
 
type IMemory<'A> (data :  list<string*float>) = 
  class
    let mutable data = data
    member s.pop p = 
      let mutable res = 0.0
      for (k,v) in data do 
        if (p = k) then 
          res <- v       
      res   
    member s.getlist = data          
    member s.push (c, v) = 
      data <- data@[(c, v)]

  end


let run (tree:Ast) (data: float list) =
  let input = new ListInput<int>(data) :> IInput
  let memory = new IMemory<float>([])
  let rec inter tree =
    match tree with  
     |Read s -> memory.push (s, input.Read.Value)
     |Write x -> 
        let v = calc x memory.getlist
        printf "%f" v
     |Seq(l,r) -> inter l
                  inter r
     |While (e, body) ->
         while (calc e memory.getlist) > 0.0 do 
           inter body
     |If(exp, l, r) -> 
        if (calc exp memory.getlist) > 0.0 then 
          inter l
        else 
          inter r
     |Assign(c, x) -> 
        memory.push (c, calc x memory.getlist)
  inter tree   
 
  
  
 
 
[<EntryPoint>]
let main argv = 
  let fk = ";\nread\nk\n;\nread\nk\n;\nread\nk\nread\np"
  let str2 = ";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres"
  let str1 = ";\nwrite\n-\n2\n1\n;\nwrite\n1\nwrite\n1"
  let str =  ";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres"
  let str3  = ";\nread\x\n;\nread\nn\n;\nif\nx\n;\nwrite\n+\nx\n1\nwrite\n*\nn\n2\nwrite\nx"
  let sfg = ";\nread\nx\n;\nwrite\nx\nread\np\n;\nread\nt"
  let please = "if\nx\n;\nwrite\n+\nx\n1\nwrite\n*\nn\n2\nwrite\nx"
  printfn "%A" (stparser str)
  run (stparser str) [3.0;3.0]  
  
  0
 
