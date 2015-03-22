type Node<'A> = 
  |Empty
  |Cons of 'A*Node<'A>

type SList<'A> = 
  interface
    abstract Size: int
    abstract Getval:     Option<'A> 
    abstract InsertHead: 'A -> unit
    abstract InsertTail: 'A -> unit
    abstract InsertIn:   'A ->int-> unit
    abstract DeleteHead: unit -> unit
    abstract DeleteTail: unit -> unit
    abstract DeleteBy:   int-> unit
    abstract Search:     ('A->bool) -> Option<'A>
    abstract concat:     SList<'A> -> unit
    abstract Print:      unit -> unit
  end

type AdtList<'A> () = 
  class 
    let mutable list = Empty
    member s.Size2 =
        let rec count node =
          match node with 
          |Empty -> 0
          |Cons(value, next) -> 1 + count next
        count list

    interface SList<'A> with
      
      member s.Getval =
        match list with
        |Empty -> None
        |Cons(v, _) -> Some v
    
      member s.Size =
         let rec count node =
           match node with 
           |Empty -> 0
           |Cons(value, next) -> 1 + count next
         count list

      member s.InsertHead value =
        list<-(Cons(value, list))
          
      member s.InsertTail value = 
        let rec insert value node = 
          match node with
          |Empty -> Cons(value, Empty)
          |Cons(v, next) -> Cons(v, insert value next)
        list<- insert value list
      
      member s.InsertIn v number =
        if (number < 0) || (number >= s.Size2) then
          failwith "Wrong index"
        else
          let rec insert node v index =
            match node with
            | Empty -> Cons(v, Empty)
            | Cons(vl, next) ->
              match compare index number with
              | c when c < 0 -> Cons(vl, insert next v (index + 1))
              | _ -> Cons(v, node)
          list <- insert list v 0
    
      member s.DeleteHead () = 
        
        match list with 
        |Empty-> failwith "List is empty"
        |Cons(value, next) -> list<-next
              
      member s.DeleteTail ()= 
        let rec delete node = 
          match node with 
          |Empty -> failwith "List is empty"
          |Cons(value, Cons(value', Empty))-> Cons(value, Empty)
          |Cons(value, next) -> Cons(value, delete next)
        list <- delete list
      
      member s.DeleteBy number =  
        if (number> s.Size2 - 1) || (number < 0)then  
          failwith "Wrong index"
        elif (number = 0) then 
          (s:>SList<'A>).DeleteHead()
        elif (number = s.Size2 - 1) then
          (s :> SList<'A>).DeleteTail() 
        else
          let rec delete n node =
            match node with 
            |Cons(value, Cons(_, next))->
              if n = number - 1 then 
                Cons(value, next)
              else 
                Cons(value, delete  (n + 1) next)
            |_ -> Empty
          list <- delete 0 list
    
      member s.Search f = 
        let rec find node = 
          match node with
          |Empty -> None
          |Cons(value, next) -> if f value then Some(value)
                                 else find next
        find list
      member s.concat l=
        
        let rec trans (l : SList<'A>) =
          match l.Size with
          |0 -> Empty
          |_ -> 
            let n = l.Getval.Value
            l.DeleteHead()
            Cons(n, trans l)

        let rec zip list1 list2 =
          match list1 with 
          |Empty -> list2
          |Cons(value, next) -> Cons(value, zip next list2)
        list<-zip list (trans l)
    
      member s.Print() = 
        printf "["
        let rec printl list =
          match list with
          |Empty -> printf ""
          |Cons(v, next) -> (printl next)
                            printf "%A;" v
        printl list
        printf "]\n"
    override s.ToString() =
      let rec itos n =
        match n with
        | Empty -> "]"
        | Cons(vl, Empty) -> vl.ToString() + " ]"
        | Cons(vl, nxt) -> vl.ToString() + "; " + (itos nxt)
      "[ " + (itos list)
             
   end 




type ArrayList<'A>()  = 
  class
    let mutable list = [||]
    interface SList<'A> with 
      member s.Size = list.Length
      member s.Getval = 
        match list with
        | [||] -> None
        | _ -> Some list.[0]

      member s.InsertTail value = 
        list <- Array.append list (Array.create 1 value)
      
      member s.InsertHead value = 
       list<- Array.append (Array.create 1 value) list
      
      member s.InsertIn value number = 
        if(number < 0) || (number > list.Length) then
          failwith "Wrong index"
        else 
          list <- Array.append (Array.append (Array.sub list 0 number) (Array.create 1 value))
                      (Array.sub list number (list.Length - number))
           
      member s.DeleteHead () = 
        let size = list.Length
        match size with
        |0 -> failwith "List is empty"
        |_ -> list <- Array.sub list 1 (size - 1)
      
      member s.DeleteTail () = 
        let size = list.Length
        match size with
        |0 -> failwith "List is empty"
        |_ -> list <- Array.sub list 0 (size - 1)           
        
      member s.DeleteBy number = 
        let size = list.Length
        if(number < 0) || (number > size) then
          failwith "Wrong index"
        else 
          match size with
          | 0 -> failwith "List empty"
          |_ -> list <- Array.append (Array.sub list 0 (number))
                          (Array.sub list (number + 1) (size - number - 1)) 
     
      member s.Search f = 
        Array.tryFind f list
      
      member s.concat array2 =
        let rec trans (array2:SList<'A>) = 
          let arr = [||]
          match array2.Size with 
          |0 -> list
          |_ -> 
             let v = array2.Getval.Value
             array2.DeleteHead()
             Array.append  (trans array2) (Array.create 1 v)
        list <- trans array2
        
      member s.Print () = 
        printfn "%A" list
    
  end 
 
[<EntryPoint>]
let main argv = 
  let mutable arr = [|12;3;4|]
  printfn "arr = %A" arr
  let mutable list1 = AdtList<int>() :> SList<int>;
  printfn "list = %A\n" list1
  list1.InsertHead 7 
  list1.Print() 
  list1.InsertTail 1 
  printfn "list <- list.InsertTail 1 = %A\n" list1
  list1.InsertIn 2 1 
  printfn "list <- lissy1.InsertIn 2 1 = %A\n" list1
  list1.InsertHead 0
  printfn "list <- list.InsertHead 0 = %A\n" list1
  list1.DeleteHead()
  printfn "list = list.DeleteHead() = %A\n" list1
  list1.DeleteTail()
  printfn "list <- list.DeleteTail() = %A\n" list1
  list1.DeleteBy 1
  printfn "list <- list.DeleteBy 1 = %A\n" list1
  list1.InsertHead 2
 
  let mutable list2 = AdtList<int>() :> SList<int>
  printf "list2 ="
  list2.InsertHead 3
  printfn "list2 <- list.InsertHead 2 = %A\n" list2
  printfn "list2.Seach ((=) 3) = %A"  (list2.Search ((=) 3))
  list1.concat list2
  printfn "list <- list.Unit list2 = %A\n" list1
  
 
  //Реализация с помощью массива
  let mutable list1 = ArrayList<int>() :> SList<int>
  list1.Print()
  list1.InsertHead 7
  printfn " l1 <- l1.InsertHead 7 = \n" 
  list1.Print()
  list1.InsertTail 1
  printfn "list <- list.InsertTail 1 = \n" 
  list1.Print()
  list1.InsertIn 2 1
  printfn "list <- list1.InsertIn 2 1 = \n" 
  list1.Print()
  list1.InsertHead 0
  printfn "list <- list.InsertHead 0 = \n" 
  list1.Print()
  list1.DeleteHead()
  printfn "list = list.DeleteHead() = \n" 
  list1.Print()
  list1.DeleteTail()
  printfn "list <- list.DeleteTail() =\n" 
  list1.Print()
  list1.DeleteBy 1
  printfn "list <- list.DeleteBy 1 = \n" 
  list1.Print()
  list1.InsertHead 2
  printfn "list <-list1.InsertHead 2 ="
  list1.Print()

  let mutable list2 = ArrayList<int>() :> SList<int>
  list2.InsertHead 3
  printfn "list2 <- list.InsertHead 2 = \n" 
  list2.Print()
  printfn "list2.Seach ((=) 3) = %A"  (list2.Search ((=) 3))
  list1.concat list2
  printfn "list <- list.Unit list2 = \n "  
  list1.Print()
  
  0 // возвращение целочисленного кода выхода
