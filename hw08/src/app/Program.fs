open System.Windows.Forms 
open Calculator
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


let list = [] 
let programLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(0,0)
  lbl.AutoSize <- true
  lbl


let NumberButton n = 
  let but = new Button()
  but.Text <- n.ToString()
  let c = coor n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e ->  
    expr <- expr + n.ToString() 
    programLabel.Text <- 
      stack.print + expr
    )
  but

let OperButton n = 
  let but = new Button()
  but.Text <- n
  let c = coor2 n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e -> 
    if n = "=" then 
      let res = calculator expr [] 
      programLabel.Text  <-
        sprintf "%f" res
      expr <- ""
    else
      expr <- expr + n
      programLabel.Text <-
         stack.print + expr
    )
  but
let BraketButton n = 
  let but = new Button()
  but.Text <- n.ToString()
  let c = coor n
  but.Location <- System.Drawing.Point (fst c, snd c)
  but.Click.Add  (fun e ->  
    expr <- expr + n.ToString() 
    programLabel.Text <- 
      stack.print + expr
    )
  but
let CleanButton =
  let but = new Button()
  but.Text <- "C"
  but.Location <- System.Drawing.Point (0, 120)
  but.Click.Add (fun _ ->
    expr <- ""
    programLabel.Text <- sprintf "%s" ""
    stack.Empty
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
  let list = ["+"; "-"; "*"; "/"; "=";"(";")"]
  let dislist = [(0,0);(0,30)]
  form.Controls.Add (programLabel)
  for i = 0 to 9 do 
    form.Controls.Add (NumberButton(i))
  for i in list do 
    form.Controls.Add (OperButton(i))
  form

[<EntryPoint>]

let main argv = 
    mainForm.Visible <- true 
    Application.Run()
    printfn "%s" expr
    0 