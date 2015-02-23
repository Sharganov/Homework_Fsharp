//Howework 1
//Realisation of peano numbers
//By Sharganov Artem
//Оценочное время: 30 минут
//Итоговое время:  30 - 40 минут в общем, а так затянулось на два вечера :)

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
  | a, S b -> plus (mult a b) a

let rec pow a b = 
  match a, b with 
  |a, Zero -> S Zero
  |Zero, _ -> Zero
  |a, S b ->mult(pow a b) a
  
let rec ToInt a = 
 match a with 
  |Zero -> 0
  |S a -> 1 + ToInt a


[<EntryPoint>]
let main argv =
  printf "%A\n"(pow(S(S(Zero))) (S(S(S(Zero)))))
  0 
