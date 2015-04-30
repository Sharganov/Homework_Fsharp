module Test
open NUnit.Framework
open Program

[<TestCase (4, [|3;4;6;24;4;2;23;34;5;6;7;6;54|], Result = 54)>]
[<TestCase (1, [|3;4;6;24;4;2;23;34;5;6;7;6;54|], Result = 54)>]
[<TestCase (2, [|0;0;0;0;0;0;0|], Result = 0)>]
[<TestCase (3, [|9;5;2;3;4;5;6;7;8|], Result = 9)>]
[<TestCase (5, [|9;5;2;3;4;5;6;7;8|], Result = 9)>]

let test threadNum arr =
  (maxInArray threadNum arr).Value

(*
Запуск с различными входными данными:
(maxInArray 1 [|for i in 0..10000000 -> i|]) ==> 2.118
(maxInArray 4 [|for i in 0..10000000 -> i|]) ==> 2.108
(maxInArray 8 [|for i in 0..10000000 -> i|]) ==> 2.603
(maxInArray 1 [|for i in 0..100000000 -> i|]) ==> 19.567
(maxInArray 4 [|for i in 0..100000000 -> i|]) ==> 18.261
*)

let check (arr: 'a []) = 
  if arr = [||] then true
  else 
    let length = arr.Length - 1
    let mutable flag = true
    let mutable i = 0
    while i < (length - 1)  && flag do
      if arr.[i] <= arr.[i+1] then 
        i<- i+1
        flag <- true
      else flag <- false
    flag
     
[<TestCase (1, [|9;8;7;6;5;4;3;2;1;0|], Result = true)>]
[<TestCase (4, [|0;1;2;3;4;5|], Result = true)>]
[<TestCase (2, [|0;1;0;1;0|], Result = true)>]
[<TestCase (3, ([||] :int []), Result = true)>]
let test2 n arr = 
  pmergesort n arr |> check
  
(*Запуски на больших данных:
pmergesort 1 [|for i in 0..1000000 -> 1000000 - i|] ==> 2.843
pmergesort 2 [|for i in 0..1000000 -> 1000000 - i|] ==> 1.666
pmergesort 4 [|for i in 0..1000000 -> 1000000 - i|] ==> 1.342
pmergesort 8 [|for i in 0..1000000 -> 1000000 - i|] ==> 1.347
pmergesort 1 [|for i in 0..10000000 -> 10000000 - i|] ==> 33.476
pmergesort 2 [|for i in 0..10000000 -> 10000000 - i|] ==> 23.369
pmergesort 4 [|for i in 0..10000000 -> 10000000 - i|] ==> 15.462
*)
