module Test
open NUnit.Framework
open Program

[<TestCase (4, [|3;4;6;24;4;2;23;34;5;6;7;6;54|], Result = 54)>]
[<TestCase (1, [|3;4;6;24;4;2;23;34;5;6;7;6;54|], Result = 54)>]
[<TestCase (2, [|0;0;0;0;0;0;0|], Result = 0)>]
[<TestCase (3, [|9;5;2;3;4;5;6;7;8|], Result = 9)>]
[<TestCase (5, [|9;5;2;3;4;5;6;7;8|], Result = 9)>]

let test threadNum arr =
  maxInArray threadNum arr

(*
Запуск с различными входными данными:
(maxInArray 1 [|for i in 0..10000000 -> i|]) ==> 2.118
(maxInArray 4 [|for i in 0..10000000 -> i|]) ==> 2.108
(maxInArray 8 [|for i in 0..10000000 -> i|]) ==> 2.603
(maxInArray 1 [|for i in 0..100000000 -> i|]) ==> 19.567
(maxInArray 4 [|for i in 0..100000000 -> i|]) ==> 18.261

*)