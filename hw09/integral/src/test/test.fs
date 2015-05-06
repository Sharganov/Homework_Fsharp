module test
open Program
open NUnit.Framework


let round x = 
 float (int (x*100.0)) / 100.0


[<TestCase ("x", 1, 1, 4, Result = 0.0)>]
[<TestCase ("x^2", 0, 1, 1, Result = 0.33)>]// 02.201
[<TestCase ("x^2", 0, 1, 2, Result = 0.33)>]// 01.758
[<TestCase ("x^2", 0, 1, 4, Result = 0.33)>]// 01.308

[<TestCase ("x^(1/2)", 0, 1, 1, Result = 0.66)>] //04.244
[<TestCase ("x^(1/2)", 0, 1, 2, Result = 0.66)>] //03.056
[<TestCase ("x^(1/2)", 0, 1, 4, Result = 0.66)>] //02.479

[<TestCase ("x^3 + x^2", 1, 2, 1, Result = 6.08)>]//04.794
[<TestCase ("x^3 + x^2", 1, 2, 2, Result = 6.08)>]//03.992
[<TestCase ("x^3 + x^2", 1, 2, 4, Result = 6.08)>]//03.226
[<TestCase ("x^3 + x^2", 1, 2, 5, Result = 6.08)>]//03.079
[<TestCase ("x^3 + x^2", 1, 2, 8, Result = 6.08)>]//03.082

let test f l r n =
   round (integral f l r n)

