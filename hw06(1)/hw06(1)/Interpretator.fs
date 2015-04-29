module Interpretator
open StParser
open ExeParser

type IInput =
  interface 
    abstract Read : Option<float>
  end 

type UserInput() = 
  class
    interface IInput with
      member s.Read =
        let s = System.Console.ReadLine()
        Some (System.Convert.ToDouble s)
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

 type Output() = 
   class
     let mutable list = []
     member s.push a = list <- a::list 
     member s.getl = list
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
        data <- List.append data [(c, v)]

  end


let runtest (tree:Ast) (data: float list) =
  let output = new Output()
  let input = new ListInput<int>(data) :> IInput
  let memory = new IMemory<float>([])
  let rec inter tree =
    match tree with  
     |Read s -> 
        memory.push (s, input.Read.Value)
     |Write x -> 
        let v = calc x memory.getlist
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
  
let run (tree:Ast) =
  let input = new UserInput() :> IInput
  let memory = new IMemory<float>([])
  let rec inter tree =
    match tree with  
     |Read s -> 
       printfn "%s" s
       memory.push (s, input.Read.Value)
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
 
 