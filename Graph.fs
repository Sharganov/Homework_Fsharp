type IGraph<'A> =
  interface 
    abstract size : int
    abstract IsWay: int->int->bool
  end 

type MatrIGraph<'A> (vertix : 'A [], Edges : list<int*int>) =
  class
    let size = vertix.Length
    let matrix = Array2D.create size size false
    do 
      for (v1, v2) in Edges do Array2D.set matrix  v1 v2 true
    member s.print = printf "%A" matrix
    interface IGraph<'A> with
      member s.size = Array2D.length1 matrix
      member s.IsWay v1 v2 = 
        Array2D.get matrix v1 v2   
  end 

type ListIGraph<'A> (vertix : 'A [], Edges : list<int*int>) = 
  class
    let size = vertix.Length
    let adjlist = Array.create size List.Empty
    do 
      for (v1, v2) in Edges do 
        Array.set adjlist v1 (v2::Array.get adjlist v1) 
    interface IGraph<'A> with 
      member s.size = adjlist.Length
      member s.IsWay v1 v2 =
       if (v1 < size) && (v2 < size) then
         List.exists ((=) v2) (Array.get adjlist v1)
       else
         false
  end 
  //достижимые вершины
let IsReachedFrom graph index = 
  let visited = Array.create (graph :> IGraph<'A>).size false
  visited.[index] <- true
  let rec find (visited :bool[]) gr cur = 
    let mutable res = []
    for i = 0 to ((gr :> IGraph<'A>).size - 1) do
      if ((gr :> IGraph<'A>).IsWay cur i) then 
        if (visited.[i] = false) then 
          visited.[i] <- true
          res <- i :: res @ (find visited gr i)
    res
  List.sort (find visited graph index)
  
let IsReachedFor graph init = 
  let rec possible (visited :bool[]) gr cur target = 
    let mutable flag = false
    for i = 0 to ((gr :> IGraph<'A>).size - 1) do
      if ((gr :> IGraph<'A>).IsWay cur i) then 
        if (visited.[i] = false) then 
          if (i = target) then flag <- true
          else
            visited.[i] <- true
            flag <- flag || (possible visited gr i target)
    flag
  let mutable result = []
  for i = 0 to ((graph :> IGraph<'A>).size - 1) do
    if not (i = init) then
       let visited = Array.create (graph :> IGraph<'A>).size false
       visited.[i] <- true
       if (possible visited graph i init) then result <- i :: result
  List.sort result

type LabelGraph<'A> = 
  interface
    inherit IGraph<'A>
    abstract WayLenght: int->int->int 
   end

let vulnerability OS = 
    match OS with
    | "Windows" -> 0.756
    | "Linux"   -> 0.110
    | "OS X"    -> 0.281
    | _         -> failwith "Incorrect OS"

type Computer (number, OS) =
    class
        let mutable infected = false

        member this.number = number
        member this.OS = OS
        member this.IsInfected = infected
        
        member this.infect (chance) = 
            if chance <= vulnerability (OS) then infected <- true

        member this.info () = 
            if infected then printfn "%d. Infected" number 
            else printfn "%d. Not infected" number
    end

type Localka (List : string list, label : bool array, list : (int list) array) =
    class
        let mutable move = 0
        let n = List.Length
        let comp = [|for i in [0 .. n - 1] -> new Computer(i, List.[i])|]

        do
            for i = 0 to n - 1 do
                if label.[i] then comp.[i].infect(0.0)

        member this.infectedNumber () =
            let mutable answer = 0
            for i in comp do
                if i.IsInfected then answer <- answer + 1
            answer

        member this.status () =
            printf "Move: %d\nStatus:\n" move 
            for i in comp do i.info()
            printf "\n\n"
            match System.Console.ReadKey().Key with
            | _ -> ()

        member this.start () =
            move <- move + 1
            let inf = Array.filter (fun i -> comp.[i].IsInfected) [|0 .. n - 1|]
            for i in inf do
                for j in list.[i] do
                    let rand = System.Random().NextDouble()
                    comp.[j].infect(rand)
    end


[<EntryPoint>]
let main argv =
   let graph = new MatrIGraph<int> ([|0;1;2;3;4;5;6;7|],
                 [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)])
   let graph' = new ListIGraph<int> ([|0;1;2;3;4;5;6;7|],
                   [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)])

   printf"REACHED VERTIX: %A\n" (IsReachedFrom graph 1 )
   printf"REACHED VERTIX: %A\n" (IsReachedFrom graph' 1)

   printf "\nReached from vertix: %A\n" (IsReachedFor graph 4)
   printf "Reached from vertix: %A\n" (IsReachedFor graph 4)

   let OSList = [ "Windows"; "Windows"; "Linux"; "OS X"; "Linux"; "Windows"; 
        "Linux"; "Windows"; "OS X" ]
   let labels = [| false; true; false; false; false; false; false; true; false |]
   let aList = [| [1;]; [0; 2;]; [1; 3; 4;]; [2;]; [2; 5;]; [4;]; [7; 8;]; [6;]; [6;] |]
   let network = new Localka (OSList, labels, aList)
   network.status()
   while network.infectedNumber() < OSList.Length do
       network.start()
       network.status()
   0 
