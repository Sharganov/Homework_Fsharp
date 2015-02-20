/*Howework 1
Realisation of peano numbers
By Sharganov Artem
*/
type peano = Zero|S of peano

let rec plus a b = 
  match a with
  |Zero->b 
  |S a -> S(plus a b) 

let rec minus a b = 
  match a, b with 
  |Zero, _ -> Zero
  |a, Zero ->a
  |S a, S b ->  minus a b
  
let rec mult a b =  
  match a, b with  
  |Zero, _ -> Zero
  |_, Zero -> Zero
  |S a, S b -> plus (mult (S a) b) (S a)

let rec pow a b = 
  match a, b with 
  |a, Zero -> S Zero
  |Zero, _ -> Zero
  |S a, S b ->mult(pow (S a) b) (S a)
  
let rec ToInt a = 
 match a with 
  |Zero -> 0
  |S a -> 1 + ToInt a


[<EntryPoint>]
let main argv =
  printf "%A\n"(pow(S(S(Zero))) (S(S(S(Zero)))))
  0 // âîçâđŕůĺíčĺ öĺëî÷čńëĺííîăî ęîäŕ âűőîäŕ
