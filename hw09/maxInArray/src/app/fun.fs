module Program

open System.Threading

let maxInRange (arr : int []) l r : int =
  let mutable res = 0
  for i in l .. r do
    if arr.[i] > res then 
      res <- arr.[i]
  res

let maxInArray threadNumber (array : int []) =
  let arraySize = array.Length
  if threadNumber <= 0 then 
     printfn "Not enough threads"
     None
  else
      let res = ref 0
      let step = arraySize / threadNumber
      let apend = arraySize % threadNumber
      let threadArray = Array.init threadNumber (fun i ->
          new Thread(ThreadStart(fun _ ->
              let threadRes = maxInRange array (i * step) ((i+1) * step + apend - 1 )
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
      Some res.Value


let split (arr : 'a array) =
    let n = arr.Length
    arr.[0..n/2-1], arr.[n/2..n-1]
 
let rec merge (arr1 : 'a array) (arr2 : 'a array) =
    let n = arr1.Length + arr2.Length
    let res = Array.zeroCreate<'a> n
    let mutable j = 0
    let mutable i = 0
    for k = 0 to n-1 do
      if i >= arr1.Length then res.[k] <- arr2.[j]; j <- j + 1
      elif j >= arr2.Length then res.[k] <- arr1.[i]; i <- i + 1
      elif arr1.[i] < arr2.[j] then res.[k] <- arr1.[i]; i <- i + 1
      else res.[k] <- arr2.[j]; j <- j + 1
 
    res
 
let rec pmergesort (threadMaxNumber : int) (arr : int []) =
    match arr with 
    | [||] -> [||]
    | [|a|] -> [|a|]
    | arr ->
       let (l,r) = split arr
       if threadMaxNumber >1 then
         let r1 = ref [||]
         let r2 = ref [||]   
        
         let mutable  res = Array.zeroCreate (l.Length + r.Length)
         let rThread = new Thread(ThreadStart(fun _ ->
                r1 := pmergesort (threadMaxNumber / 2) l
               ))
         rThread.Start()
         r2 := pmergesort (threadMaxNumber / 2) r
         rThread.Join()
         merge r1.Value r2.Value
       else 
         merge  (pmergesort (threadMaxNumber / 2) l) (pmergesort (threadMaxNumber/2) r)
   
[<EntryPoint>]
let main argv = 
    0 
