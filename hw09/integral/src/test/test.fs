module test
open Program
open NUnit.Framework


let round x = 
 float (int (x*100.0)) / 100.0


[<TestCase ("x", 1, 1, 4, Result = 0.0)>]
[<TestCase ("x^2", 0, 1, 1, Result = 0.33)>]// 13.104
[<TestCase ("x^2", 0, 1, 2, Result = 0.33)>]// 07.751
[<TestCase ("x^2", 0, 1, 4, Result = 0.33)>]// 06.400

[<TestCase ("x^(1/2)", 0, 1, 1, Result = 0.66)>] //23.824
[<TestCase ("x^(1/2)", 0, 1, 2, Result = 0.66)>] //14.628
[<TestCase ("x^(1/2)", 0, 1, 4, Result = 0.66)>] //12.229

[<TestCase ("x^3 + x^2", 1, 2, 1, Result = 6.08)>]//26.187
[<TestCase ("x^3 + x^2", 1, 2, 2, Result = 6.08)>]//17.249
[<TestCase ("x^3 + x^2", 1, 2, 4, Result = 6.08)>]//13.861
[<TestCase ("x^3 + x^2", 1, 2, 5, Result = 6.08)>]//15.178
[<TestCase ("x^3 + x^2", 1, 2, 8, Result = 6.08)>]//15.453

let test f l r n =
   round (integral f l r n)

