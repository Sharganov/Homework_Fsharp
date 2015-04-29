module Program
open System.Threading

exception NoThreads

let maxInRange (arr : int []) l r : int =
  let mutable res = 0
  for i in l .. r do
    if arr.[i] > res then 
      res <- arr.[i]
  res

let maxInArray threadNumber (array : int []) =
  let arraySize = array.Length
  if threadNumber <= 0 then raise NoThreads
  let res = ref 0
  let step = arraySize / threadNumber
  let apend = arraySize % threadNumber
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = maxInRange array (i * step) ((i+1) * step + apend - 1)
          Monitor.Enter(res)
          if threadRes > res.Value then 
            res := threadRes
          Monitor.Exit(res)
               ))
    )
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

[<EntryPoint>]
let main argv = 
    try 
      printfn "Example: maxInArray 4 [|for i in 0..1000000 -> i|]"
      printfn "%d" (maxInArray 4 [|for i in 0..1000000 -> i|])
    with
    | NoThreads ->    
      printfn "The number of threads should be more at least than one"
    0 
