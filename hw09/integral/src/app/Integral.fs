module Program
open Calculator 
open System.Threading
let square (a:float) b h  =  (a+b)*h/2.0

let spliting = 5000000//"частота" разбиения

(*
Функции integal1 и count1 не дают преимущество во времени  при данном воде,
 а дают более точный результат (так как разбиние каждого отрезка
 становится более мелким при большем количестве потоков, а колчиество  операций 
 возрастает.
*)

let count1 (l:float) r f  = 
  let step = (r - l)/float spliting
  let mutable res = 0.0
  for i= 0 to spliting do 
    let v1 = calculator f (l + (float i)*step)
    let v2 = calculator f (l + float((i+1))*step)
    res <- res + square v1 v2 step
  res

let integral1 f (l:float) r threadNum  = 
  let res = ref 0.0
  let step = (r - l)/ float threadNum
  let threadArray = Array.init threadNum (fun i ->
    new Thread(ThreadStart(fun _ ->
    let threadRes = count1 (l + (float i)*step) (l+float(i+1)*step) f 
    Monitor.Enter(res)
    res := res.Value + threadRes 
    Monitor.Exit(res)
      ))
    )
  for i in threadArray do 
    i.Start()
  for i in threadArray do 
    i.Join()
  res.Value
     


let count (l:float) r f x = 
  let step = (r - l)/float x
  let mutable res = 0.0
  for i= 0 to x do 
    let v1 = calculator f (l + (float i)*step)
    let v2 = calculator f (l + float((i+1))*step)
    res <- res + square v1 v2 step
  res

let integral f (l:float) r threadNum  =
  let res = ref 0.0
  let step = (r - l)/ float threadNum
  let threadArray = Array.init threadNum (fun i ->
    new Thread(ThreadStart(fun _ ->
    let threadRes = count (l + (float i)*step) (l+float(i+1)*step) f (spliting/threadNum)
    Monitor.Enter(res)
    res := res.Value + threadRes 
    Monitor.Exit(res)
      ))
    )
  for i in threadArray do 
    i.Start()
  for i in threadArray do 
    i.Join()
  res.Value
     
[<EntryPoint>]
let main argv =  
    if (argv.Length < 4) then printfn "Not Enough arguments"
    else
      printfn "%f" (integral argv.[0] (float argv.[1]) (float argv.[2]) (int argv.[3]))
    0 
