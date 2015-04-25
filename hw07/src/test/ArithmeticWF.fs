module ArithmeticWF

type RingBuilder(n:int) = 
  member this.Return x = 
    let p = x%n
    if p >= 0 then p
    else p+n
  member this.Bind (x, f) = f(x%n)

