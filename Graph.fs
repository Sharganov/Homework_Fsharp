
type IGraph =
  interface 
    abstract size : int
    abstract IsWay: int->int->bool
  end 
type IlabledGraph<'A> = 
  inherit IGraph
  abstract Marks : bool []
  abstract Vertix : 'A []
  
type MatrIGraph<'A> (vertix : 'A [], Edges : list<int*int>) =
  class
    let size = vertix.Length
    let matrix = Array2D.create size size false
    do 
      for (v1, v2) in Edges do Array2D.set matrix  v1 v2 true
    member s.print = printf "%A" matrix

    interface IGraph with
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
    member s.print = printfn "%A" adjlist
    interface IGraph with 
      member s.size = adjlist.Length
      member s.IsWay v1 v2 =
       if (v1 < size) && (v2 < size) then
         List.exists ((=) v2) (Array.get adjlist v1)
       else
         false
  end 
  //достижимые вершины
let IsReachedFrom (graph : IGraph) index = 
  let visited = Array.create graph.size false
  visited.[index] <- true
 
  let rec find (visited :bool[]) (gr:IGraph) current = 
    let mutable res = []
    for i = 0 to gr.size - 1 do
      if (gr.IsWay current i) then 
        if (visited.[i] = false) then 
          visited.[i] <- true
          res <- List.append (i :: res) (find visited gr i)
    res
  List.sort (find visited graph index)
  
let IsReachedFor (graph: IGraph) init = 
  let rec possible (visited :bool[]) (gr:IGraph) cur target = 
    let mutable flag = false
    for i = 0 to (gr.size - 1) do
      if (gr.IsWay cur i) then 
        if (visited.[i] = false) then 
          if (i = target) then flag <- true
          else
            visited.[i] <- true
            flag <- flag || (possible visited gr i target)
    flag
  let mutable result = []
  
  for i = 0 to (graph.size - 1) do
    if not (i = init) then
       let visited = Array.create graph.size false
       visited.[i] <- true
       if (possible visited graph i init) then result <- i :: result
  List.sort result

let Probability OS = 
  match OS with
  | "Windows" -> 0.647
  | "Linux"   -> 0.310
  | "OS X"    -> 0.281
  | _         -> failwith "Incorrect OS"

type Computer (number, OS) =
  class
    let mutable infected = false
    member s.number = number
    member s.OS = OS
    member s.isInfected () = infected
    member s.infect (probability) = 
      if probability <= Probability (OS) then infected <- true
    member s.info () = 
      if infected then printfn "%d: Infected" number 
      else printfn "%d: Not infected" number
  end

type ComputerNet (OSlist : string list, labels: bool array, Edges:list<int*int> ) =
  class
    let size = OSlist.Length
    let list = [|for i in [0 .. size - 1] ->  new Computer(i, OSlist.[i])|]
    let matrix = Array2D.create size size false
    do 
      for (v1, v2) in Edges do Array2D.set matrix  v1 v2 true

    interface IlabledGraph<Computer> with
      member s.size = size
      member s.Vertix = list
      member s.IsWay v1 v2 = 
        Array2D.get matrix v1 v2  
      member s.Marks = labels    
  end

type Net(net : IlabledGraph<Computer>) = 
  class
    let mutable move = 0
    let n = net.size - 1
    do 
      for i = 0 to n do
        if net.Marks.[i] then 
          net.Vertix.[i].infect(0.0)
    member s.NumberOfInf = 
      let mutable result = 0
      for i in net.Vertix do
        if i.isInfected() then result <- result + 1
      result
   
    member s.start =
      move <- move + 1
      let inf = Array.filter (fun i -> net.Vertix.[i].isInfected()) [|0 .. n|]
      for i in inf do
        for j = 0 to n do
          if net.IsWay net.Vertix.[i].number net.Vertix.[j].number then
            let rand = System.Random().NextDouble()
            net.Vertix.[j].infect(rand)
      for i = 0 to n do
        if net.Vertix.[i].isInfected() then net.Marks.[i] <- true
    member s.status () =
      printf "\n\nMove: %d\nStatus:\n" move
      let c = [|for i in net.Vertix -> if i.isInfected() then '!' else ' '|]
      printf "
      Windows%c --  Linux%c -- OS X%c
      |                |
      |                |
      Linux%c   --  Linux %c -- Windows%c
          
         
      Linux%c  --   Windows%c
      |               |
      |               |
      OS X %c   --  Linux%c
      \n\n" c.[0] c.[1] c.[2] c.[3] c.[4] c.[5] c.[6] c.[7] c.[8] c.[9]
      printf "\nPress any key to continue . . . "
      match System.Console.ReadKey().Key with | _ -> ()

    
  end
  
[<EntryPoint>]
let main argv =
   printfn "Input datas: \n(|0;1;2;3;4;5;6;7|) - list of vertix \n\n"
   printfn "[(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)]) - list of  adjacent vertix\n"
   let graph = new MatrIGraph<int> ([|0;1;2;3;4;5;6;7|],
                 [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)])
   let graph2 = new ListIGraph<int> ([|0;1;2;3;4;5;6;7|],
                   [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)])
   printfn "Adjacent matrix for graph:\n"
   graph.print
   printfn "\n\nAdjacent list for graph:\n"
   graph2.print
   printfn "\nRealisation with matrix:\n"
   printfn"Reached vertix 1: %A" (IsReachedFrom graph 1 )
   printf "Reached from vertix 4: %A\n" (IsReachedFor graph 4)
   printfn "\nRealisation with list: \n"   
   printf "Reached from vertix 4: %A\n" (IsReachedFor graph2 4)
   printfn"Reached vertix 1: %A\n" (IsReachedFrom graph2 1)

   printfn "__________________\n"
   printfn "Task 26: "
   printfn " ! - computer is infected\n --- means link in both sides "
   let OSList = 
        [ "Linux"; "Windows"; "Linux"; "OS X"; "Linux"; "Windows"; 
            "Linux"; "Windows"; "OS X"; "Linux"]
   let edges = [(0,1);(0,4);(1,2);(1,0);(2,4);(2,3);(4,5);(6,7);
               (6,8);(7,6);(7,9);(8,6);(8,9);(9,7);(9,8)]
   let labels = [| false; true; false; false; false;
                false; false; true; false; false |]
   let graph = new ComputerNet (OSList, labels, edges)
   let localNet = new Net (graph)
   localNet.status()
   while localNet.NumberOfInf < OSList.Length do
        localNet.start
        localNet.status()




   0 
