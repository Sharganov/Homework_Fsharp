module StParser
open ExeParser

type Ast = 
       |Read  of string
       |Write  of Expr
       |Assign of string * Expr
       |Seq    of Ast * Ast
       |If     of Expr * Ast * Ast
       |While  of Expr * Ast
       
      
let stparser (str:string) = 
  let array =  str.Split '\n'
  let list = List.ofArray array
 
  let rec treeparser (slist: string list):(Ast * (string list)) = 
      let mutable slist = slist

      match slist.Head with
      |";" -> 
              let a = treeparser slist.Tail
              slist<- snd a
              let b = treeparser slist.Tail 
              (Seq (fst a,fst b), snd b) 
      |":=" ->
               let a = slist.Tail.Head
               slist <- slist.Tail
               let b = exeparser slist.Tail
               (Assign(a, fst b), snd b) 
      |"read" -> 
                 let a = slist.Tail.Head
                 slist <- slist.Tail
                 (Read a, slist)
      |"write" ->
                 let a = exeparser slist.Tail
                 (Write(fst a), snd a)
      |"if" ->
               let expr = exeparser slist.Tail
               let left = treeparser (snd expr).Tail
               let right = treeparser (snd left).Tail
               (If(fst expr,fst left,fst right), snd right) 
      |"while" ->
                 let expr = exeparser slist.Tail
                 let body = treeparser (snd expr).Tail
                 (While(fst expr, fst body), snd body)
     
  let t = treeparser list
  fst t
