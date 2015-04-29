module test
open SCalculator
open NUnit.Framework

[<TestCase ("1 - 2 - 3", Result = -4.0)>]
[<TestCase ("3 ^ 1 ^ 2", Result = 3.0)>]
[<TestCase ("2 + 2 * 2", Result = 6.0)>]
[<TestCase ("3 - ( - 2 )", Result = 5.0)>]
[<TestCase ("( 17 / 34 ) ^ 5 * 32", Result = 1.0)>]
[<TestCase ("12 / ( 2 * 3 )", Result = 2)>]
[<TestCase ("19 + ( 2 % 4 ) ^ 2", Result = 23.0)>]
[<TestCase ("( 19 + 18 * ( 2 + 6 ) / 3 ^ 2 ) % 4", Result = 3.0)>]
[<TestCase ("( 3 + 4 ) ^ 2 - ( 5 - 7 ) ^ 3", Result = 57.0)>]
let test value = 
  calculator value 
  