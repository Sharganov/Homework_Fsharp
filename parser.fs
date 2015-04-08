module program
open Calculator


let check (str:string) = 
  match str with
  |"while" -> false
  |"if" -> false
  |"write" -> false
  |"read" ->  false
  |":=" -> false
  |_ -> true 
  
type Ast = 
       |Read   of Ast
       |Write  of Ast
       |Assign of Ast * Ast
       |Seq    of Ast * Ast
       |If     of Ast * Ast * Ast
       |While  of Ast * Ast
       |Empty
       |Num of float
       |Oper of char*Ast*Ast
       |Var of string

let tr (x: char) = 
  match x with 
  |';' -> Seq(Empty, Empty)   

let mutable i = 0 
let parser (str:string) = 
  let array =  str.Split '\n'
  let list = List.ofArray array
  let stack = new Stack<Ast>()
 
  let rec treeparser (slist: string list):(Ast * (string list)) = 
      let mutable slist = slist

      match slist.Head with
      |";" -> 
              let a = treeparser slist.Tail
              slist<- snd a
              let b = treeparser slist.Tail 
              (Seq (fst a,fst b), snd b) 
      |":=" ->
               let a = treeparser slist.Tail
               let b = treeparser (snd a).Tail
               (Assign(fst a, fst b), snd b) 
      |"read" -> 
                 let a = treeparser slist.Tail
                 (Read( fst a), snd a)
      |"write" ->
                 let a = treeparser slist.Tail
                 (Write(fst a), snd a)
      |"if" ->
               let expr = treeparser slist.Tail
               let left = treeparser (snd expr).Tail
               let right = treeparser (snd left).Tail
               (If(fst expr,fst left,fst right), snd right) 
      |"while" ->
                 let expr = treeparser slist.Tail
                 let body = treeparser (snd expr).Tail
                 (While(fst expr, fst body), snd body)
      |"+" -> 
              let x = treeparser slist.Tail
              slist<- snd x
              let b = treeparser slist.Tail
              (Oper('+',fst x, fst b), snd b)
      |"-" -> 
              let x = treeparser slist.Tail
              slist<- snd x
              let b = treeparser slist.Tail
              (Oper('-',fst x, fst b), snd b)
      |"*" -> 
              let x = treeparser slist.Tail
              slist<- snd x
              let b = treeparser slist.Tail
              (Oper('*',fst x, fst b), snd b)
      |"/" -> 
              let x = treeparser slist.Tail
              slist<- snd x
              let b = treeparser slist.Tail
              (Oper('/',fst x, fst b), snd b)
      |"^" -> 
              let x = treeparser slist.Tail
              slist<-snd x
              let b = treeparser slist.Tail
              (Oper('^',fst x, fst b), snd b)
      |x -> if  System.Char.IsDigit x.[0] then 
              let v = Num (System.Convert.ToDouble x)
              (v, slist)
            else 
              (Var x, slist)
  let t = treeparser list
  fst t

[<EntryPoint>]
let main argv =
    printfn "%A" (parser (";\nread\nx\nwrite\n+\nx\n1\n"))
    printfn "%A" argv
    0 
