module Interpreter
open StParser
open ExeParser
open IOStream

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
        data <- List.append data [(c, v)]

  end
 
let run (str:string) =
  let tree = stparser str
  let input = new UserInput() :> IInput
  let output = new Output()
  let memory = new IMemory<float>([])
  let rec inter tree =
    match tree with  
     |Read s -> 
       printfn "Enter: %s" s
       memory.push (s, input.Read.Value)
     |Write x -> 
        let v = calc x memory.getlist  
        printf "%f" v
        output.push v
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
  output.getl
 