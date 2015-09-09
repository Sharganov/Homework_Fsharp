open System.Windows.Forms 
open SCalculator
let mutable expr = ""
let coor n = 
  match n with
  |0 -> (77,120)
  |1 -> (0,30)
  |2 -> (77,30)
  |3 -> (154,30)
  |4 -> (0,60)
  |5 -> (77,60)
  |6 -> (154,60)
  |7 -> (0,90)
  |8 -> (77,90)
  |9 -> (154,90)



let coor2 n = 
  match n with 
  |"+"-> (231,30)
  |"-"-> (231,60)
  |"*"-> (231,90)
  |"/"-> (231,120)
  |"="-> (154,120)
  |"("-> (308, 30)
  |")"-> (308, 60)
  |"sin" ->(385, 60)
  |"cos" -> (385, 90)
  |"^" -> (308, 90)
  |"," -> (308, 120)
let list = [] 
let programLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(0,0)
  lbl.AutoSize <- true
  lbl

let PiNumber =
  let but = new Button()
  but.Text <-System.Math.PI.ToString()
  but.Location <- System.Drawing.Point (385, 30)
  but.Click.Add (fun e ->
    expr <- expr + but.Text
    programLabel.Text  <- expr
    )
  but

let NumberButton n = 
  let but = new Button()
  but.Text <- n.ToString()
  let c = coor n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e ->  
    expr <- expr + n.ToString() 
    programLabel.Text <- expr
    )
  but

let OperButton n = 
  let but = new Button()
  but.Text <- n
  let c = coor2 n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e -> 
    match n with 
    | "=" -> 
      try
        expr <- (calculator expr).ToString()  
      with 
       |NoRightRb -> expr <- "No right bracket!\nPress C and try again"
       |_ -> expr <- "Invalid expressoin\nPress C and try again"
      programLabel.Text  <-
        sprintf "%s" expr
    | "sin" -> 
       expr <- (System.Math.Sin (calculator expr)).ToString()
       programLabel.Text  <-
        sprintf "%s" expr
    | "cos" ->
       expr <- (System.Math.Cos (calculator expr)).ToString()
       programLabel.Text  <-
        sprintf "%s" expr
    |"-" -> 
      if expr.Length > 0 && expr.[expr.Length - 1] = '(' then 
         expr <- expr + " " + n           
      else
         expr <- expr + " " + n + " "
      programLabel.Text <- expr
    | "," ->
       expr <- expr + n 
       programLabel.Text <- expr
    | _ ->
      expr <- expr + " " + n + " "
      programLabel.Text <- expr
    )
  but

let BraketButton (n:string) = 
  let but = new Button()
  but.Text <- n.ToString()
  let c = coor2 n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e ->  
    match n with 
    | ")" ->
      expr <- expr + " " + n.ToString() 
      programLabel.Text <-  sprintf "%s" expr
    | "(" -> 
      expr <- expr +  n.ToString() + " "
      programLabel.Text <-  sprintf "%s" expr
    )
  but
 
let clean1Button = 
  let but = new Button()
  but.Text <- "<-"
  but.Location <- System.Drawing.Point (308, 0)
  but.Click.Add (fun _ ->
    if (expr.Length > 0 ) then 
      expr <- expr.[0..expr.Length - 2]
      programLabel.Text <- sprintf "%s" expr
    )
  but
let CleanButton =
  let but = new Button()
  but.Text <- "C"
  but.Location <- System.Drawing.Point (0, 120)
  but.Click.Add (fun _ ->
    expr <- ""
    programLabel.Text <- sprintf "%s" ""
    )
  but
let exitButton (form: Form) = 
  let but = new Button()
  but.Text <- "Exit"
  but.Location <- System.Drawing.Point (100, 200)
  but.AutoSize <- true
  but.Click.Add (fun e -> Application.Exit())
  but

let mainForm  = 
  let form = new Form(Visible = false)
  form.Controls.Add (exitButton(form))
  form.Controls.Add (CleanButton)
  form.Controls.Add (clean1Button)
  form.Controls.Add (PiNumber)
  let list = ["+"; "-"; "*"; "/"; "=";"sin";"cos";"^";","]
  let list2 = ["(";")"]
  let dislist = [(0,0);(0,30)]
  form.Controls.Add (programLabel)
  for i = 0 to 9 do 
    form.Controls.Add (NumberButton(i))
  for i in list do 
    form.Controls.Add (OperButton(i))
  for i in list2 do 
    form.Controls.Add (BraketButton(i))
  form

[<EntryPoint>]

let main argv = 
    mainForm.Visible <- true
    Application.Run()
    printfn "%s" expr
    0 