type Node<'A> = 
  |Empty
  |Cons of 'A*Node<'A>

type SList<'A> = 
  interface
    abstract Size: int
    abstract Getval: Option<'A> 
    abstract InsertHead: 'A -> SList<'A>
    abstract InsertTail: 'A -> SList<'A>
    abstract InsertIn:   'A -> int -> SList<'A>
    abstract DeleteHead: unit -> SList<'A>
    abstract DeleteTail: unit -> SList<'A> 
    abstract DeleteBy:   int-> SList<'A>
    abstract Search:     ('A->bool) -> Option<'A>
    abstract Unit: SList<'A> -> SList<'A>
  end

type AdtList<'A> (head : Node<'A>) = 
  class  
    new(value) = AdtList(Cons(value, Empty)) 
    
     member s.Size2 =
        let rec count node =
          match node with 
          |Empty -> 0
          |Cons(value, next) -> 1 + count next
        count head

    interface SList<'A> with
      
      member s.Getval =
        match head with
        |Empty -> None
        |Cons(v, _) -> Some v
    
      member s.Size =
         let rec count node =
           match node with 
           |Empty -> 0
           |Cons(value, next) -> 1 + count next
         count head

      member s.InsertHead value =
        new AdtList<'A> (Cons(value, head)) :> SList<'A>  
       
      member s.InsertTail value = 
        let rec insert value node = 
          match node with
          |Empty -> Cons(value, Empty)
          |Cons(v, next) -> Cons(v, insert value next)
        AdtList<'A>(insert value head) :> SList<'A>
    
      member s.InsertIn v number =
        if (number < 0) || (number >= s.Size2) then
          failwith "Wrong index!"
        else
          let rec insert node v index =
            match node with
            | Empty -> Cons(v, Empty)
            | Cons(vl, next) ->
              match compare index number with
              | c when c < 0 -> Cons(vl, insert next v (index + 1))
              | _ -> Cons(v, node)
          AdtList<'A>(insert head v 0 ):> SList<'A>  
    
      member s.DeleteHead ()= 
        match head with 
        |Empty->s :> SList<'A>
        |Cons(value, next) -> 
           AdtList<'A>(next) :> SList<'A>
      
      member s.DeleteTail ()= 
        let rec delete node = 
          match node with 
          |Empty -> Empty
          |Cons(value, Cons(value', Empty))-> Cons(value, Empty)
          |Cons(value, next) -> Cons(value, delete next)
        AdtList<'A>(delete head):>SList<'A>
      
      member s.DeleteBy number=  
        if (number> s.Size2 - 1) || (number < 0)
        then failwith "Wrong Index"
        if (number = 0) then 
          (s :> SList<'A>).DeleteHead()
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
          AdtList<'A>( delete 0 head):> SList<'A>
    
      member s.Search f = 
        let rec find node = 
          match node with
          |Empty -> None
          |Cons(value, next) -> if f value then Some(value)
                                 else find next
        find head
      member s.Unit l=
        let rec trans (l : SList<'A>) =
          match l.Size with
          |0 -> Empty
          |_ -> Cons(l.Getval.Value, trans(l.DeleteHead()))
        let rec zip list1 list2 =
          match list1 with 
          |Empty -> list2
          |Cons(value, next) -> Cons(value, zip next list2)
        AdtList<'A>( zip head (trans l)):> SList<'A>
    
    override s.ToString() =
      let rec itos n =
        match n with
        | Empty -> "]"
        | Cons(vl, Empty) -> vl.ToString() + " ]"
        | Cons(vl, nxt) -> vl.ToString() + "; " + (itos nxt)
      "[ " + (itos head)
   end




type ArrayList<'A> (a: 'A [])  = 
  class
    let array = a
    let size = a.Length
    new() = ArrayList<'A>(Array.empty<'A>)
    new(value : 'A)= ArrayList<'A>(Array.create 1 value)

    interface SList<'A> with 

      member s.Size = size
     
      member s.Getval = 
        match array with
        | [||] -> None
        | _ -> Some array.[0]

      member s.InsertTail value = 
        ArrayList<'A>(Array.append array (Array.create 1 value)) :> SList<'A>
      
      member s.InsertHead value = 
        ArrayList<'A>(Array.append (Array.create 1 value) array) :> SList<'A>
      
      member s.InsertIn value number = 
        let arr = Array.append (Array.append (Array.sub array 0 number) (Array.create 1 value))
                    (Array.sub array number (size - number + 1))
        ArrayList<'A>(arr) :>  SList<'A>
      
      member s.DeleteHead () = 
        match size with
        |0 -> (s :> SList<'A>) 
        |_ -> ArrayList<'A>(Array.sub array 1 (size - 1)) :> SList<'A>
      
      member s.DeleteTail () = 
        match size with
        |0 -> (s :> SList<'A>)
        |_ -> ArrayList<'A>(Array.sub array 0 (size - 1)) :> SList<'A>
           
        
       member s.DeleteBy number = 
        if(number < 0) || (number > size) then
          failwith "Wrong index"
        else 
          match size with
          | 0 -> (s :> SList<'A>)
          |_ ->
            ArrayList<'A>(Array.append (Array.sub array 0 (number))
                            (Array.sub array (number + 1) (size - number - 1))) :> SList<'A> 
     
      member s.Search f = 
        Array.tryFind f array
      
      member s.Unit array2 =
        let rec trans (array2:SList<'A>) = 
          match array2.Size with 
          |0 -> [||]
          |_ -> Array.append ( Array.create 1 array2.Getval.Value)
                 (trans (array2.DeleteHead()))
        ArrayList<'A>(Array.append  array (trans array2)):>SList<'A>
        
       
  end 

[<EntryPoint>]
let main argv = 
  let mutable list1 = AdtList<int>(3) :> SList<int>;
  printfn "list = %A\n" list1
  list1 <- list1.InsertHead 7
  printfn " l1 <- l1.InsertHead 7 = %A\n" list1
  list1 <- list1.InsertTail 1
  printfn "list <- list.InsertTail 1 = %A\n" list1
  list1 <- list1.InsertIn 2 1
  printfn "list <- lissy1.InsertIn 2 1 = %A\n" list1
  list1 <- list1.InsertHead 0
  printfn "list <- list.InsertHead 0 = %A\n" list1
  list1 <- list1.DeleteHead()
  printfn "list = list.DeleteHead() = %A\n" list1
  list1 <- list1.DeleteTail()
  printfn "list <- list.DeleteTail() = %A\n" list1
  list1 <- list1.DeleteBy 1
  printfn "list <- list.DeleteBy 1 = %A\n" list1
  let mutable list2 = list1.InsertHead 2 
  printfn "list2 <- list.InsertHead 2 = %A\n" list2
  printfn "list2.Seach ((=) 3) = %A"  (list2.Search ((=) 3))
  list1 <- list1.Unit list2
  printfn "list <- list.Unit list2 = %A\n" list1
  
  //Реализация с помощью массива
  let mutable list1 = AdtList<int>(3) :> SList<int>
  printfn "list = %A\n" list1
  list1 <- list1.InsertHead 7
  printfn " l1 <- l1.InsertHead 7 = %A\n" list1
  list1 <- list1.InsertTail 1
  printfn "list <- list.InsertTail 1 = %A\n" list1
  list1 <- list1.InsertIn 2 1
  printfn "list <- list1.InsertIn 2 1 = %A\n" list1
  list1 <- list1.InsertHead 0
  printfn "list <- list.InsertHead 0 = %A\n" list1
  list1 <- list1.DeleteHead()
  printfn "list = list.DeleteHead() = %A\n" list1
  list1 <- list1.DeleteTail()
  printfn "list <- list.DeleteTail() = %A\n" list1
  list1 <- list1.DeleteBy 1
  printfn "list <- list.DeleteBy 1 = %A\n" list1
  let mutable list2 = list1.InsertHead 2 
  printfn "list2 <- list.InsertHead 2 = %A\n" list2
  printfn "list2.Seach ((=) 3) = %A"  (list2.Search ((=) 3))
  list1 <- list1.Unit list2
  printfn "list <- list.Unit list2 = %A\n " list1
  0 // возвращение целочисленного кода выхода
