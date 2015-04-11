open NUnit.Framework
open FsUnit
 
type Node<'A> = 
  |Empty
  |Cons of 'A*Node<'A>

type SList<'A> = 
  interface
    abstract Size: int
    abstract Getval:     Option<'A> 
    abstract InsertHead: 'A -> bool
    abstract InsertTail: 'A -> bool
    abstract InsertIn:   'A ->int-> bool
    abstract DeleteHead: unit -> bool
    abstract DeleteTail: unit -> bool
    abstract DeleteBy:   int-> bool
    abstract Search:     ('A->bool) -> Option<'A>
    abstract concat:     SList<'A> -> bool
    abstract Print:      unit -> unit
    abstract GetRes :    unit -> string // for testing AdtList
    abstract GetRes2:    unit -> 'A array

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
        list<-(Cons(value, list)); true 
          
      member s.InsertTail value =        
         let rec insert value node = 
           match node with
           |Empty -> Cons(value, Empty)
           |Cons(v, next) -> Cons(v, insert value next)
         list<- insert value list;true
      
      member s.InsertIn v number =
        if (number < 0) || (number >= s.Size2) then
          false
        else
          let rec insert node v index =
            match node with
            | Empty -> Cons(v, Empty)
            | Cons(vl, next) ->
              match compare index number with
              | c when c < 0 -> Cons(vl, insert next v (index + 1))
              | _ -> Cons(v, node)
          list <- insert list v 0; true
    
      member s.DeleteHead () = 
        
        match list with 
        |Empty-> false
        |Cons(value, next) -> list<-next; true
              
      member s.DeleteTail ()= 
         if s.Size2 = 0 then 
          false
         else
           let rec delete node = 
             match node with 
             |Empty -> Empty
             |Cons(value, Cons(value', Empty))-> Cons(value, Empty)
             |Cons(value, next) -> Cons(value, delete next)
           list <- delete list; true
      
      member s.DeleteBy number =  
        if s.Size2 = 0 then false
        elif (number> s.Size2 - 1) || (number < 0)then  
          false
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
          list <- delete 0 list; true
    
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
            ignore(l.DeleteHead())
            Cons(n, trans l)

        let rec zip list1 list2 =
          match list1 with 
          |Empty -> list2
          |Cons(value, next) -> Cons(value, zip next list2)
        list<-zip list (trans l); true
    
      member s.Print() = 
        printf "["
        let rec printl list =
          match list with
          |Empty -> printf ""
          |Cons(v, next) -> (printl next)
                            printf "%A;" v
        printl list
        printf "]\n"

      member s.GetRes() = 
        let rec itos n =
          match n with
          | Empty -> "]"
          | Cons(vl, Empty) -> vl.ToString() + " ]"
          | Cons(vl, nxt) -> vl.ToString() + "; " + (itos nxt)
        "[ " + (itos list)
      member s.GetRes2() =
        failwith "This can`t be used for testing of AdtList"
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
        list <- Array.append list (Array.create 1 value); true
      
      member s.InsertHead value = 
       list<- Array.append (Array.create 1 value) list; true
      
      member s.InsertIn value number = 
        if(number < 0) || (number > list.Length) then
          false
        else 
          list <- Array.append (Array.append (Array.sub list 0 number) (Array.create 1 value))
                      (Array.sub list number (list.Length - number)); true
           
      member s.DeleteHead () = 
        let size = list.Length
        match size with
        |0 -> false
        |_ -> list <- Array.sub list 1 (size - 1); true
      
      member s.DeleteTail () = 
        let size = list.Length
        match size with
        |0 -> false
        |_ -> list <- Array.sub list 0 (size - 1); true           
        
      member s.DeleteBy number = 
        let size = list.Length
        if(number < 0) || (number > size) then
          false
        else 
          match size with
          | 0 -> false
          |_ -> list <- Array.append (Array.sub list 0 (number))
                          (Array.sub list (number + 1) (size - number - 1)); true
     
      member s.Search f = 
        Array.tryFind f list
      
      member s.concat array2 =
        let rec trans (array2:SList<'A>) = 
          let arr = [||]
          match array2.Size with 
          |0 -> list
          |_ -> 
             let v = array2.Getval.Value
             ignore(array2.DeleteHead())
             Array.append  (trans array2) (Array.create 1 v)
        list <- trans array2; true
      member s.GetRes () = failwith "This can`t be used for testing of AdtList"
      member s.GetRes2 () = list
      member s.Print () = 
        printfn "%A" list
    
  end 

[<Test>]
let ``Insert in head of list (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertTail 8)
  Assert.AreEqual(true, list.InsertHead 1)

[<Test>]
let ``Insert in head of empty list (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  Assert.AreEqual(true, list.InsertHead 1)

[<Test>]
let ``Insert in tail (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 1)
  Assert.AreEqual(true, list.InsertTail 1)

[<Test>]
let ``Insert in tail of empty list (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  Assert.AreEqual(true, list.InsertTail 1)

[<Test>]
let ``InsertIn out of range (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  Assert.AreEqual(false, list.InsertIn 1 10)

[<Test>]
let ``Normal InsertIn (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  ignore(list.InsertTail 5)
  Assert.AreEqual(true, list.InsertIn 1 1)

[<Test>]
let ``DeleteHead in empty list (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  Assert.AreEqual (false, list.DeleteHead())
[<Test>]
let ``DeleteHead in list (array)`` () =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 1)
  Assert.AreEqual (true, list.DeleteHead())
[<Test>]
let ``DeleteTail in empty list (array)`` ()= 
  let list = new ArrayList<int>() :> SList<int>
  Assert.AreEqual (false, list.DeleteTail())
[<Test>]
let ``DeleteTail in list (array)`` ()= 
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (true, list.DeleteTail())
[<Test>]
let ``DeleteBy in list (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  ignore(list.InsertHead 5)
  ignore(list.InsertHead 5)
  Assert.AreEqual (true, list.DeleteBy 1)
[<Test>]
let ``DeleteBy in list out range (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (false, list.DeleteBy 4)
[<Test>]
let ``Search in list correct (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (Some 5, list.Search ((=) 5))
[<Test>]
let ``Search in list incorrect (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (None, list.Search ((=) 3))
[<Test>]
let ``Concat list (array)``() =
  let list = new ArrayList<int>() :> SList<int>
  let list1 = new ArrayList<int>() :> SList<int> 
  ignore(list.InsertHead 5)
  ignore(list1.InsertHead 5)
  Assert.AreEqual (true, list.concat list1)

[<Test>]
let ``Insert in head of list`` () =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertTail 8)
  Assert.AreEqual(true, list.InsertHead 1)

[<Test>]
let ``Insert in head of empty list`` () =
  let list = new AdtList<int>() :> SList<int>
  Assert.AreEqual(true, list.InsertHead 1)

[<Test>]
let ``Insert in tail`` () =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 1)
  Assert.AreEqual(true, list.InsertTail 1)

[<Test>]
let ``Insert in tail of empty list`` () =
  let list = new AdtList<int>() :> SList<int>
  Assert.AreEqual(true, list.InsertTail 1)

[<Test>]
let ``InsertIn out of range`` () =
  let list = new AdtList<int>() :> SList<int>
  Assert.AreEqual(false, list.InsertIn 1 10)

[<Test>]
let ``Normal InsertIn``() =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  ignore(list.InsertTail 5)
  Assert.AreEqual(true, list.InsertIn 1 1)

[<Test>]
let ``DeleteHead in empty list`` () =
  let list = new AdtList<int>() :> SList<int>
  Assert.AreEqual (false, list.DeleteHead())
[<Test>]
let ``DeleteHead in list `` () =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 1)
  Assert.AreEqual (true, list.DeleteHead())
[<Test>]
let ``DeleteTail in empty list`` ()= 
  let list = new AdtList<int>() :> SList<int>
  Assert.AreEqual (false, list.DeleteTail())
[<Test>]
let ``DeleteTail in list`` ()= 
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (true, list.DeleteTail())
[<Test>]
let ``DeleteBy in list``() =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  ignore(list.InsertHead 5)
  ignore(list.InsertHead 5)
  Assert.AreEqual (true, list.DeleteBy 1)
[<Test>]
let ``DeleteBy in list out range``() =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (false, list.DeleteBy 4)
[<Test>]
let ``Search in list correct``() =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (Some 5, list.Search ((=) 5))
[<Test>]
let ``Search in list incorrect``() =
  let list = new AdtList<int>() :> SList<int>
  ignore(list.InsertHead 5)
  Assert.AreEqual (None, list.Search ((=) 3))

[<Test>]
let ``Concat list``() =
  let list = new AdtList<int>() :> SList<int>
  let list1 = new AdtList<int>() :> SList<int> 
  ignore(list.InsertHead 5)
  ignore(list1.InsertHead 5)
  Assert.AreEqual (true, list.concat list1)
 
[<Test>]
let ``Geneeal functions of Adtlist `` () = 
  let mutable list1 = AdtList<int>() :> SList<int>;
  ignore(list1.InsertHead 7) 
  ignore(list1.InsertTail 1) 
  ignore(list1.InsertIn 2 1) 
  ignore(list1.InsertHead 0)
  ignore(list1.DeleteHead())
  ignore(list1.DeleteTail())
  ignore(list1.DeleteBy 1)
  ignore(list1.InsertHead 2)
  Assert.AreEqual ( "[ 2; 7 ]", list1.GetRes())

[<Test>]
let ``General functions of arraylist`` () =
  let mutable list1 = ArrayList<int>() :> SList<int>;
  ignore(list1.InsertHead 7) 
  ignore(list1.InsertTail 1) 
  ignore(list1.InsertIn 2 1) 
  ignore(list1.InsertHead 0)
  ignore(list1.DeleteHead())
  ignore(list1.DeleteTail())
  ignore(list1.DeleteBy 1)
  ignore(list1.InsertHead 2)
  Assert.AreEqual ( [| 2; 7 |], list1.GetRes2())


[<EntryPoint>]
let main argv =
  0
