//Standart functions for list
//Made by Sharganov Atrem
//Предполагаемое время: 1 вечер
//Получилось: 1 вечер

let reverseList list = 
  List.fold (fun acc i -> i::acc) [] list

let map f list = 
  reverseList (List.fold (fun acc i -> f i::acc) [] list)

let filter f list = 
  reverseList( List.fold (fun acc i ->  if (f i) then i::acc else acc) [] list)

let horner value cof = 
  List.fold (fun x acc -> acc + x*value) 0 cof

[<EntryPoint>]
let main argv =
  let x = [1 .. 10]
  printfn "x = %A\n" x 
  printfn "Reverse x : %A\n" (reverseList x)
  printfn "Map (fun x-> x + 5) x: %A\n" (map (fun x -> x + 5) x)
  printfn "Filter (fun x-> x > 6) x : %A\n" (filter (fun x -> x > 6) x)
  printfn "Task 4:\nx = %d\ncoef: %A\nReslut: %A" 7 x (horner 5 [1..5])
  0
