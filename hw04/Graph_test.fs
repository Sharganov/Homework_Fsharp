open NUnit.Framework
open FsUnit

type IGraph =
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

type IlabledGraph<'A> = 
  inherit IGraph
  abstract Marks : bool []
  abstract Vertix : 'A []

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
   
    member s.start v = // for testing
      move <- move + 1
      let inf = Array.filter (fun i -> net.Vertix.[i].isInfected()) [|0 .. n|]
      for i in inf do
        for j = 0 to n do
          if net.IsWay net.Vertix.[i].number net.Vertix.[j].number then
            let rand = System.Random().NextDouble() + v
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
[<TestFixture>]
type GraphTest () = 
  let edges =  [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)]
  let edges' = [(0,1);(0,2);(0,3);(0,4);(1,0);(1,1);(1,2);(1,3);(1,4);(2,0);(2,1);(2,2);(2,3);
                   (2,4);(3,0);(3,1);(3,2);(3,3);(3,4);(4,0);(4,1);(4,2);(4,3)]
  let newMatrGraph edge = new MatrIGraph<int> ([|0;1;2;3;4;5;6;7|], edge)
  let newListGraph edge = new ListIGraph<int> ([|0;1;2;3;4;5;6;7|], edge)
  
  [<Test>]
  member s.``Is Reached From (matrix)``() = 
    let graph = newMatrGraph edges
    Assert.AreEqual ([2;3;4;5;6;7], IsReachedFrom graph 1)
  [<Test>]
  member s.``Is Reached From (list)`` () = 
    let graph =  newListGraph edges
    Assert.AreEqual ([2;3;4;5;6;7], IsReachedFrom graph 1)
  [<Test>]
  member s.``Is Reached From (matrix, full)``() = 
    let graph = newMatrGraph edges'
    Assert.AreEqual ([0;2;3;4], IsReachedFrom graph 1)
  [<Test>]
  member s.``Is Reached From (list, full)``() = 
    let graph = newListGraph edges'
    Assert.AreEqual ([0;2;3;4], IsReachedFrom graph 1)
  [<Test>]
  member s.``Is Reached From (matrix, empty)``() = 
    let graph = newMatrGraph []
    Assert.AreEqual ([], IsReachedFrom graph 1)
  [<Test>]
  member s.``Is Reached From (list, empty)``() =
    let graph = newListGraph []
    Assert.AreEqual ([], IsReachedFrom graph 1) 
  [<Test>]
  member s.``Is Reached For (matrix) ``() = 
    let graph = newMatrGraph edges
    Assert.AreEqual ([0;1;2;3], IsReachedFor graph 4)
  [<Test>]
  member s.``Is Reached For (list) ``() = 
    let graph = newListGraph edges
    Assert.AreEqual ([0;1;2;3], IsReachedFor graph 4)
  [<Test>]
  member s.``Is Reached For (matrix, empty) ``() = 
    let graph = newMatrGraph []
    Assert.AreEqual ([], IsReachedFor graph 1)
  [<Test>]
  member s.``Is Reached For (list, empty) ``() = 
    let graph = newListGraph []
    Assert.AreEqual ([], IsReachedFor graph 1)
  [<Test>]
  member s.``Is Reached For (matrix, full)``() = 
    let graph = newMatrGraph edges'
    Assert.AreEqual ([0;2;3;4], IsReachedFor graph 1)
  [<Test>]
  member s.``Is Reached For (list, full)``() = 
    let graph = newListGraph edges'
    Assert.AreEqual ([0;2;3;4], IsReachedFor graph 1)


let OSList = 
        [ "Linux"; "Windows"; "Linux"; "OS X"; "Linux"; "Windows"; 
            "Linux"; "Windows"; ]
let labels = [| false; true; false; false; false; false; false; true; |]

let list = [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)]


[<TestCase ( 1.0, Result =  2)>] //0% to be infected
[<TestCase (-1.0, Result = 7)>]  // 100% to be infected
let ``Test 01 for local network`` value =
    let graph = new ComputerNet (OSList, labels, list) :> IlabledGraph<Computer>
    let network = new Net (graph)
    let mutable n = 0
    if value = -1.0 then n <- 2 else n <- 10000
    for i = 0 to n do 
        network.start value
    network.NumberOfInf  
 
[<EntryPoint>]
let main argv =
   0 
