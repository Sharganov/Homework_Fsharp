module test
open Program
open NUnit.Framework


let round x = 
 float (int (x*100.0)) / 100.0


[<TestCase ("x", 1, 1, 4, Result = 0.0)>]
[<TestCase ("x^2", 0, 1, 1, Result = 0.33)>]// 51мс
[<TestCase ("x^2", 0, 1, 2, Result = 0.33)>]// 35мс
[<TestCase ("x^2", 0, 1, 4, Result = 0.33)>]// 30мс

[<TestCase ("x^(1/2)", 0, 1, 1, Result = 0.66)>] //106мс
[<TestCase ("x^(1/2)", 0, 1, 2, Result = 0.66)>] //61мс
[<TestCase ("x^(1/2)", 0, 1, 4, Result = 0.66)>] //43мс

[<TestCase ("x^3 + x^2", 1, 2, 1, Result = 6.08)>]//102мс
[<TestCase ("x^3 + x^2", 1, 2, 2, Result = 6.08)>]//77мс
[<TestCase ("x^3 + x^2", 1, 2, 4, Result = 6.08)>]//50мс
[<TestCase ("x^3 + x^2", 1, 2, 8, Result = 6.08)>]//58мс

let test f l r n =
   round (integral f l r n)

