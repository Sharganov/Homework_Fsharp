open NUnit.Framework
open FsUnit
let pr x =
  match x with 
  |'(' -> 1
  |'+' -> 2
  |'-' -> 2
  |'/' -> 3
  |'*' -> 3
  |'^' -> 4 
  |'%' -> 3 
  |x ->  match (System.Char.IsLetter x) with
         |true  -> 0
         |false -> -1
 

type Stack<'A> = 
  interface
    abstract deletetop: unit -> bool
    abstract push : 'A -> bool 
    abstract pop  : unit -> Option<'A>
    abstract isEmpty : unit -> bool
    abstract size : unit -> int
    abstract top  : unit -> Option<'A>
  end

type ListStack<'A> () = 
  class
    let mutable list = []
    interface Stack<'A> with 
      member s.deletetop () = 
        if list.Length = 0 then false
        else list <- list.Tail; true
      member s.size () = list.Length
      member s.push a = list <- a ::list; true
      member s.isEmpty() = list.IsEmpty 
      member s.top () = 
        if list.IsEmpty then None else Some list.Head 
      member s.pop () = 
        if list.IsEmpty
        then None
        else 
          let head = list.Head
          list <- list.Tail
          Some head
  end


let toPostfix (input:string) =
  let stack =  new ListStack<char>():>Stack<char>
  let mutable output = ""
  let mutable i = 0
  let mutable lenght = input.Length - 1
  while lenght >= 0 do
     match input.[i] with
     |')' -> while(stack.top().Value <> '(') do 
               output <- output + stack.pop().Value.ToString()
               output <- output + ' '.ToString()
             ignore(stack.deletetop())
             i <- i + 1
             lenght <- lenght - 1
            
     | _ ->  
               if (System.Char.IsDigit input.[i])||pr input.[i] = 0 then 
                 output <- output + input.[i].ToString()
                 i <- i + 1
                 lenght <- lenght - 1
                 while (lenght >= 0)&&((System.Char.IsDigit input.[i])||pr input.[i] = 0) do 
                   output <- output + input.[i].ToString()
                   i <- i + 1
                   lenght <- lenght - 1
                 output <- output + ' '.ToString()
               elif (input.[i] = ' ') then 
                 i <- i + 1
                 lenght <- lenght - 1
               elif input.[i] = '(' then 
                 if (input.[i+1] = '-') then
                   output <- output + '-'.ToString()
                   ignore(stack.push input.[i])
                   i <- i + 2
                   lenght <- lenght - 2
                 else 
                   ignore(stack.push input.[i])
                   i <- i + 1
                   lenght <- lenght - 1
               elif stack.isEmpty() || pr input.[i] > pr (stack.top().Value) then 
                 ignore(stack.push input.[i])
                 i <- i + 1
                 lenght <- lenght - 1
               elif pr input.[i] <= pr (stack.top().Value) then
                 while  (stack.isEmpty()<>true)&&(pr input.[i] <= pr (stack.top().Value)) do
                   output <- output + stack.pop().Value.ToString()
                 ignore(stack.push input.[i])
                 output<- output + ' '.ToString()
                 i <- i + 1
                 lenght <- lenght - 1
              
  while (stack.isEmpty() <> true) do 
     output <- output + stack.pop().Value.ToString() + ' '.ToString()
  output

let smartcalc (str:string) (data : list<string*float>) = 
  let input = toPostfix str
  let stack = new ListStack<float>():>Stack<float>
  let mutable i = 0 
  let mutable lenght = input.Length - 1
  let mutable acc = ""
  let mutable flag =  false
  while (lenght > 0) do 
   match input.[i] with
     |'+' -> ignore(stack.push(stack.pop().Value + stack.pop().Value))
     |'-' -> if (input.[i+1] = ' ') then 
               let v1 = stack.pop().Value
               let v2 = stack.pop().Value
               ignore(stack.push(v2 - v1 ))
             else flag<-true
     |'*' -> ignore(stack.push(stack.pop().Value * stack.pop().Value))
     |'/' -> 
             let v1 = stack.pop().Value
             let v2 = stack.pop().Value
             ignore(stack.push(v2/v1))
     |'%' -> let v1 = stack.pop().Value
             let v2 = stack.pop().Value
             ignore(stack.push(v2%v1))

     |'^' -> let v1 = stack.pop().Value
             let v2 = stack.pop().Value
             ignore(stack.push(v2**v1))
     |' ' -> i <- i
     |number ->
        acc <- ""  
        if (System.Char.IsDigit input.[i]) then 
          while (System.Char.IsDigit  input.[i]) do 
            acc <- acc + input.[i].ToString() 
            i<- i + 1
            lenght <- lenght - 1
          if (flag = true) then    
            ignore(stack.push (- System.Convert.ToDouble acc))
            flag<-false
          else      
            ignore(stack.push (System.Convert.ToDouble acc))
        else
          while (input.[i] <> ' ') do 
            acc <- acc + input.[i].ToString()
            i<- i + 1
            lenght <- lenght - 1
          for (key, v) in data do
            if (key = acc) then
              ignore(stack.push v) 
        
   i<-i+1
   lenght<-lenght - 1
  stack.top().Value

[<TestCase ("2*2+2", Result = 6.0)>]
[<TestCase ("3-(-2)", Result = 5.0)>]
[<TestCase ("(17/34)^5*32", Result = 1.0)>]
[<TestCase ("12/(2*3)", Result = 2)>]
[<TestCase ("3+7*(1-5)^2^3/1024", Result = 31.0)>]
[<TestCase ("19+(2%4)^2", Result = 23.0)>]
[<TestCase ("(19+18*(2+6)/3^2)%4", Result = 3.0)>]
[<TestCase ("(3+4)^2 - (5 - 7)^3", Result = 57.0)>]
let test value = 
  smartcalc value []

[<TestCase ("2^x^y^2", Result = 0.0625 )>]
[<TestCase ("(2*x+5*y)^2", Result = 1)>]
[<TestCase ( "((-2*x)*(-4))^4 - number^3", Result = 1536)>]
[<TestCase ("2^y + x*number%6", Result = 2.5)>]
let test05 value = 
  smartcalc value [("x", 2.0);("y", -1.0);("number", 40.0)]
 
       
[<EntryPoint>]
let main argv = 
    0
