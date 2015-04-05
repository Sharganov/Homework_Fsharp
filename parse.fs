
type Token = 
  |Num of float
  |LB 
  |RB 
  |Oper of char
  |Con of string

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
