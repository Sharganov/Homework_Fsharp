module SCalculator

type Token = 
  |Num of float
  |LB 
  |RB 
  |Oper of char
  
let pr (s:Token) =
  match s with 
  |Oper('+') -> 2
  |Oper('-') -> 2
  |Oper('/') -> 3
  |Oper('*') -> 3
  |Oper('^') -> 4 
  |Oper('%') -> 3 
  |_-> - 1 
      
let rassoc (c:char) =
 match c with 
 |'^' -> true
 |_ -> false
 
type Stack<'A> () = 
  class
    let mutable list = []
    member s.print  =
      if list.IsEmpty then 
        ""
      else 
      sprintf "%A" list.Head
    member s.deletetop () = 
      if list.Length = 0 then false
      else list <- list.Tail; true
    member s.Empty = list <- []
    member s.size () = list.Length
    member s.push (a:'A) = list <- a ::list; true
    member s.isEmpty() = list.IsEmpty 
    member s.top () = 
      if list.IsEmpty then None else Some list.Head 
    member s.pop () = 
      if list.IsEmpty
      then None
      else 
        let head = list.Head
        list <- list.Tail
        Some head
  end

let parse (input:string) = 
  let array = input.Split ' '
  let mutable tlist = []
  let mutable i = 0
  while i < array.Length  do
      match array.[i] with 
      |" "-> ()
      |"+" -> tlist <- Oper('+')::tlist
      |"-" -> tlist <- Oper('-')::tlist
      |"/" -> tlist <- Oper('/')::tlist
      |"*" -> tlist <- Oper('*')::tlist
      |"^" -> tlist <- Oper('^')::tlist
      |"%" -> tlist <- Oper('%')::tlist
      |")" -> tlist <- RB::tlist
      |"(" -> tlist <- LB::tlist
      |x -> if (System.Char.IsDigit x.[0])||(x.[0] = '-') then 
              tlist <- Num(System.Convert.ToDouble x) :: tlist    
      i <- i+1
  tlist <- List.fold (fun acc i -> i::acc) [] tlist
  tlist
   
exception NoRightRb
exception InvalidExpressoin

let ToPostfix (str:string) =
  let mutable tlist = parse str
  let mutable result = []
  let mutable stack = new Stack<Token>()
  let mutable flag = false
  while (not tlist.IsEmpty) do 
    match tlist.Head with
    |Num(x) -> result<- Num(x)::result
    |LB -> ignore(stack.push LB)
           flag <- true
           
    |RB -> while not (stack.isEmpty()) && stack.top().Value <> LB do 
             result <- stack.pop().Value :: result
           ignore(stack.deletetop())
           flag <- false
    |Oper(s) -> 
                if rassoc s then 
                  while (not(stack.isEmpty()))&&(pr (Oper(s)) < pr (stack.top().Value)) do 
                    result<- stack.pop().Value::result
                else 
                  while (not(stack.isEmpty()))&& (pr (Oper(s)) <= pr (stack.top().Value)) do
                    result<- stack.pop().Value::result
                ignore(stack.push (Oper(s)))
    tlist <- tlist.Tail
  if flag then raise NoRightRb
  while (not (stack.isEmpty())) do 
    result<- stack.pop().Value::result
  result <- List.fold (fun acc i -> i::acc) [] result
  result

let stack = new Stack<float>()

let calculator (str:string)  = 
  try
    let mutable tlist = ToPostfix str
    while (not (tlist.IsEmpty)) do 
      match tlist.Head with
      |Num(x) -> ignore(stack.push x)
      |Oper(s) ->
                 let v1 = stack.pop().Value
                 let v2 = stack.pop().Value
                 match s with 
                 |'+' -> ignore(stack.push(v1 + v2))
                 |'-' -> ignore(stack.push(v2 - v1))
                 |'/' -> ignore(stack.push(v2 / v1))
                 |'*' -> ignore(stack.push(v1 * v2))
                 |'^' -> ignore(stack.push(v2 ** v1))
                 |'%' -> ignore(stack.push(v2 % v1))
      tlist <- tlist.Tail
  with 
    |NoRightRb -> raise NoRightRb
    |_ -> raise InvalidExpressoin
  stack.top().Value



