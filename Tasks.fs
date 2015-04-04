open System.IO
open NUnit.Framework

let ReadFrom (filename : string) = 
  use stream = new StreamReader(filename)
  let str = stream.ReadToEnd ()
  str

let WriteTo (filename : string) (str:string) = 
  use stream = new StreamWriter (filename)
  stream.Write (str)
  
let pr x =
  match x with 
  |'(' -> 1
  |'+' -> 2
  |'-' -> 2
  |'/' -> 3
  |'*' -> 3
  |'^' -> 4 
  |'%' -> 3 
  |x -> match System.Char.IsLetter x with
        |res when res = true -> 0
        |_ -> - 1


type Stack<'A> = 
  interface
    abstract deletetop: unit -> bool
    abstract push : 'A -> bool 
    abstract pop  : unit -> Option<'A>
    abstract isEmpty : unit -> bool
    abstract size : unit -> int
    abstract top  : unit -> Option<'A>
    abstract print : unit -> unit  
  end

type ListStack<'A> () = 
  class
    let mutable list = []
    interface Stack<'A> with 
      member s.print () =
        printfn "%A" list
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


let toPostfix (s : string) =
  use ostream = new StreamWriter "otest.txt"
  let input = ReadFrom s
  let stack =  new ListStack<char>():>Stack<char>
  let mutable i = 0
  let mutable lenght = input.Length - 1
  let mutable buffer = ""
  let mutable flag = false
  while lenght >= 0 do
     match input.[i] with
     |')' -> while(stack.top().Value <> '(') do 
               ostream.WriteLine (stack.pop().Value)
             ignore(stack.deletetop())
             i <- i + 1
             lenght <- lenght - 1
            
     | _ ->  
               buffer <- ""
               if (System.Char.IsDigit input.[i])||pr input.[i] = 0 then 
                 if flag = true then 
                   buffer <- buffer + "-"
                   flag <- false
                 buffer <- buffer + input.[i].ToString()
                 i <- i + 1
                 lenght <- lenght - 1
                 while (lenght >= 0)&&((System.Char.IsDigit input.[i])||pr input.[i] = 0) do 
                   buffer <- buffer + input.[i].ToString()
                   i <- i + 1
                   lenght <- lenght - 1
                 ostream.WriteLine buffer
               elif (input.[i] = ' ') then 
                 i <- i + 1
                 lenght <- lenght - 1
               elif input.[i] = '(' then 
                 if (input.[i+1] = '-') then
                   flag <- true
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
                   ostream.WriteLine (stack.pop().Value)
                 ignore(stack.push input.[i])
                 i <- i + 1
                 lenght <- lenght - 1
                      
  while (stack.isEmpty() <> true) do 
    ostream.WriteLine (stack.pop().Value)
  


let stackmachine (fn : string) = 
  use instream = new StreamReader (fn)
  use outstream = new StreamWriter "result.txt"
  let stack = new ListStack<float>():>Stack<float>
  while not instream.EndOfStream do 
    let str = instream.ReadLine()
    
    match str with 
    |"+" -> ignore(stack.push(stack.pop().Value + stack.pop().Value))
    |"-" -> if str.Length < 2 then 
               let v1 = stack.pop().Value
               let v2 = stack.pop().Value
               ignore(stack.push(v2 - v1 ))

    |"*" -> ignore(stack.push(stack.pop().Value * stack.pop().Value))
    |"/" -> 
             let v1 = stack.pop().Value
             let v2 = stack.pop().Value
             ignore(stack.push(v2/v1))
    |"%" -> let v1 = stack.pop().Value
            let v2 = stack.pop().Value
            ignore(stack.push(v2%v1))

    |"^" -> let v1 = stack.pop().Value
            let v2 = stack.pop().Value
            ignore(stack.push(v2**v1))
    |_ -> 
          ignore(stack.push (System.Convert.ToDouble str))
  outstream.Write (stack.top().Value)

[<TestCase ("2+2*2", Result = 6.0)>]
[<TestCase ("3-(-2)", Result = 5.0)>]
[<TestCase ("(17/34)^5*32", Result = 1.0)>]
[<TestCase ("12/(2*3)", Result = 2)>]
[<TestCase ("3+7*(1-5)^2^3/1024", Result = 31.0)>]
[<TestCase ("19+(2%4)^2", Result = 23.0)>]
[<TestCase ("(19+18*(2+6)/3^2)%4", Result = 3.0)>]
[<TestCase ("(3+4)^2 - (5 - 7)^3", Result = 57.0)>]
let test value = 
  WriteTo "test.txt" value 
  toPostfix "test.txt"
  stackmachine "otest.txt"
  let str = ReadFrom "result.txt"
  System.Convert.ToDouble str
[<EntryPoint>]
let main argv = 
  0 
