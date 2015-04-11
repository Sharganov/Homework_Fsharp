module IOStream

type IInput =
  interface 
    abstract Read : Option<float>
  end 

type UserInput() = 
  class
    interface IInput with
      member s.Read =
        let s = System.Console.ReadLine()
        Some (System.Convert.ToDouble s)
  end

type ListInput<'A> (data : float list) = 
  class
    let mutable data = data
    interface IInput with
      member s.Read =   
        if data.IsEmpty
        then None
        else 
          let head = data.Head
          data <- data.Tail
          Some head
  end 


 type Output() = 
   class
     let mutable list = []
     member s.push a = list <- a::list 
     member s.getl = list
   end

