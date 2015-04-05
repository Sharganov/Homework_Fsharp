open System.IO
open NUnit.Framework

type Token = 
  |Num of float
  |LB 
  |RB 
  |Oper of char
  |Con of string


let pr (s:Token) =
  match s with 
  |Oper('+') -> 2
  |Oper('-') -> 2
  |Oper('/') -> 3
  |Oper('*') -> 3
  |Oper('^') -> 4 
  |Oper('%') -> 3 
  |_-> - 1 

      
let rassoc (c:char) =
 match c with 
 |'^' -> true
 |_ -> false

let ReadFrom (filename : string) = 
  use stream = new StreamReader(filename)
  let str = stream.ReadToEnd ()
  str

let WriteTo (filename : string) (str:string) = 
  use stream = new StreamWriter (filename)
  stream.Write (str)
  

type Stack<'A> () = 
  class
    let mutable list = [] 
    member s.print () =
      printfn "%A" list
    member s.deletetop () = 
      if list.Length = 0 then false
      else list <- list.Tail; true
    member s.size () = list.Length
    member s.push (a:'A) = list <- a ::list; true
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



let parse (input : string) =
  let length = input.Length - 1
  let mutable i = 0
  let mutable buffer = ""
  let mutable tlist =  []
  let mutable flag = false
  while i <= length do
    match input.[i] with
    |'_' -> tlist<-tlist
    |'+' -> tlist <- Oper('+')::tlist
    |'-' ->if(input.[i-1] = '(') 
             then flag <- true
           else  
             tlist <- Oper('-')::tlist
    |'/' -> tlist <- Oper('/')::tlist
    |'*' -> tlist <- Oper('*')::tlist
    |'^' -> tlist <- Oper('^')::tlist
    |'%' -> tlist <- Oper('%')::tlist
    |')' -> tlist <- RB::tlist
    |'(' -> tlist <- LB::tlist
    |_ -> buffer <- ""
          while (i<=length)&&((System.Char.IsDigit input.[i])||System.Char.IsLetter input.[i]) do 
            buffer <- buffer + input.[i].ToString()
            i<-i+1
          if flag then 
            buffer <- "-" + buffer
            flag <- false
          if System.Char.IsDigit (buffer.[0])||buffer.[0] = '-' then 
            tlist <-Num(System.Convert.ToDouble buffer)::tlist
          else 
            tlist<- Con(buffer)::tlist
          i<-i-1
    i<-i+1     
  tlist <- List.fold (fun acc i -> i::acc) [] tlist
  tlist

let getval (s:Token) = 
  match s with 
  |Oper(x) -> x
  |_-> 'N'
let ToPostfix (fn:string) =
  use ostream = new StreamWriter "otest.txt"
  let str = ReadFrom fn
  let mutable tlist = parse str
  let mutable stack = new Stack<Token>()
  while (not tlist.IsEmpty) do 
    match tlist.Head with
    |Num(x) -> ostream.WriteLine x
    |Con(s) -> ostream.WriteLine s
    |LB -> ignore(stack.push LB)
    |RB -> while stack.top().Value <> LB do
            ostream.WriteLine (getval(stack.pop().Value))
           ignore(stack.deletetop())
    |Oper(s) -> 
                if rassoc s then 
                  while (not(stack.isEmpty()))&&(pr (Oper(s)) < pr (stack.top().Value)) do 
                    ostream.WriteLine (getval(stack.pop().Value))
                else 
                  while (not(stack.isEmpty()))&& (pr (Oper(s)) <= pr (stack.top().Value)) do
                    ostream.WriteLine (getval(stack.pop().Value))
                ignore(stack.push (Oper(s)))
    tlist <- tlist.Tail
  while (not (stack.isEmpty())) do 
    ostream.WriteLine (getval(stack.pop().Value))

let stackmachine (fn : string) = 
  use instream = new StreamReader (fn)
  use outstream = new StreamWriter "result.txt"
  let stack = new Stack<float>()
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

[<TestCase ("3^1^2", Result = 3.0)>]
[<TestCase ("2+2*2", Result = 6.0)>]
[<TestCase ("3-(-2)", Result = 5.0)>]
[<TestCase ("(17/34)^5*32", Result = 1.0)>]
[<TestCase ("12/(2*3)", Result = 2)>]
[<TestCase ("19+(2%4)^2", Result = 23.0)>]
[<TestCase ("(19+18*(2+6)/3^2)%4", Result = 3.0)>]
[<TestCase ("(3+4)^2-(5-7)^3", Result = 57.0)>]
let test value = 
  WriteTo "test.txt" value 
  ToPostfix "test.txt"
  stackmachine "otest.txt"
  let str = ReadFrom "result.txt"
  System.Convert.ToDouble str

[<TestCase ("4/2", Result = "4\r\n2\r\n/\r\n")>]
let test01 value = 
  WriteTo "test.txt" value 
  ToPostfix "test.txt"
  let str = ReadFrom "otest.txt"
  str

[<EntryPoint>]
let main argv = 
  0 
