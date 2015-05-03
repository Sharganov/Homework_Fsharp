module Program

open System.Threading

let maxInRange (arr : int []) l r : int =
  let mutable res = arr.[0]
  for i in l .. r do
    if arr.[i] > res then 
      res <- arr.[i]
  res
let lock x f  =
         Monitor.Enter(x)
         try
           f()  
         finally 
           Monitor.Exit(x)  

let maxInArray threadNumber (array : int []) =
  if array.Length < 0 then 
    printfn "Empty array"
    None 
  elif threadNumber <= 0 then 
     printfn "Not enough threads"
     None
  else
      let arraySize = array.Length
      let res = ref array.[0]
      let step = arraySize / threadNumber
      let append = arraySize % threadNumber
      let mutable threadRes = 0 

      let lock (res : int ref) (x: int)  =
         Monitor.Enter(res);
         try
           if res.Value < x then 
             res := x
         finally  
           Monitor.Exit(res)     
     
      if append <> 0 then 
        lock res (maxInRange array (threadNumber*step) (array.Length - 1))
      
      let threadArray = Array.init threadNumber (fun i ->
          new Thread(ThreadStart(fun _ ->
              let threadRes = maxInRange array (i * step) ((i+1) * step - 1 )
              lock res threadRes
        )))
      for t in threadArray do
        t.Start()
      for t in threadArray do
        t.Join()
      Some res.Value

let split (arr : 'a array) =
    let n = arr.Length
    arr.[0..n/2-1], arr.[n/2..n-1]
 
let rec merge (arr1 : int array) (arr2 : int array) (res : int [] ref) =
    let n = arr1.Length + arr2.Length
    let mutable j = 0
    let mutable i = 0
    for k = 0 to n-1 do
      if i >= arr1.Length then res.Value.[k] <- arr2.[j]; j <- j + 1
      elif j >= arr2.Length then res.Value.[k] <- arr1.[i]; i <- i + 1
      elif arr1.[i] < arr2.[j] then res.Value.[k] <- arr1.[i]; i <- i + 1
      else res.Value.[k] <- arr2.[j]; j <- j + 1
    res.Value
 
let rec pmergesort (threadMaxNumber : int) (arr : int []) =
    match arr with 
    | [||] -> [||]
    | [|a|] -> [|a|]
    | arr ->
       let (l,r) = split arr
       let res = ref (Array.zeroCreate (l.Length + r.Length))
       if threadMaxNumber >1 then
         let r1 = ref [||]
         let r2 = ref [||]   
        
         let rThread = new Thread(ThreadStart(fun _ ->
                r1 := pmergesort (threadMaxNumber / 2) l
               ))
         rThread.Start()
         r2 := pmergesort (threadMaxNumber / 2) r
         rThread.Join()
         lock res (fun _ -> merge r1.Value r2.Value res)
       else 
         merge  (pmergesort 0 l) (pmergesort 0 r) res

[<EntryPoint>]
let main argv = 
   0 
