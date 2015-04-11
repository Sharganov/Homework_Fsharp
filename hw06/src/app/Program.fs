module Program
open Interpreter
open System.IO

let norm (str:string) = 
  let mutable ostr = ""
  for i=0 to (str.Length - 1) do 
    match str.[i] with 
    |'\r' -> ()
    |x -> ostr <- ostr + x.ToString()
  ostr


[<EntryPoint>]
let main argv =
  if(argv.Length=0) then
    printfn "Not enough arguments"
  else 
    use input = new StreamReader (argv.[0])
    let t  = input.ReadToEnd()
    let res = run (norm t)
    if (argv.Length>1) then
      let output = new StreamWriter (argv.[1])
      output.Write res
  0
