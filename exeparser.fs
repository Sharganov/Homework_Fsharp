module ExeParser

type Expr = 
       |Num of float
       |Oper of char*Expr*Expr
       |Var of string

let rec exeparser (slist: string list):(Expr * (string list)) = 
  let mutable slist = slist
  match slist.Head with 
   |"+" -> 
              let x = exeparser slist.Tail
              slist<- snd x
              let b = exeparser slist.Tail
              (Oper('+',fst x, fst b), snd b)
      |"-" -> 
              let x = exeparser slist.Tail
              slist<- snd x
              let b = exeparser slist.Tail
              (Oper('-',fst x, fst b), snd b)
      |"*" -> 
              let x = exeparser slist.Tail
              slist<- snd x
              let b = exeparser slist.Tail
              (Oper('*',fst x, fst b), snd b)
      |"/" -> 
              let x = exeparser slist.Tail
              slist<- snd x
              let b = exeparser slist.Tail
              (Oper('/',fst x, fst b), snd b)
      |"^" -> 
              let x = exeparser slist.Tail
              slist<-snd x
              let b = exeparser slist.Tail
              (Oper('^',fst x, fst b), snd b)
      |x -> if  System.Char.IsDigit x.[0] then 
              let v = Num (System.Convert.ToDouble x)
              (v, slist)
            else 
              (Var x, slist)


let calc (t :Expr) (data : list<string*float>)= 
 
  let rec cl tree  =
     let mutable result = 0.0
     match tree with
     |Num x -> x
     |Var x -> for (key, v) in data do 
                 if key = x then 
                   result <- v
               result      
     |Oper(s, exp1, exp2) ->
        match s with
          |'+' -> cl exp1 + cl exp2 
          |'-' -> cl exp1 - cl exp2
          |'/' -> cl exp1 / cl exp2
          |'*' -> cl exp1 * cl exp2
          |'^' -> cl exp1 ** cl exp2
          |'%' -> cl exp1 % cl exp2
  cl t

