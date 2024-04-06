(* file: main.ml
   author: Jack Edwards
 
   This is a simulation that places photoreceptor cells in
   the unit circle. There are 20X more rods than cones. Of
   the latter, 64% are red, 33% are green and 3% are blue.
 
  Usage:
   > cd src/simple
   > dune exec bin/main.exe
*)


(* Boilerplate Global Constants *)

let displayWidth = 800.
let displayHeight = displayWidth
let radius = displayWidth /. 2.0
let cellRadius = 3.0
let clockRate = 0.01
let veryDarkGray = Color.makeColor 64 64 64
let backing = Image.circle radius Color.white
let text = Image.text "Click" veryDarkGray ~size: 50.0
let splash  = Image.placeImage text (radius -. 50., radius -. 20.) backing
 
type state = Ready | Running
 
(* My Code Starts Here *)

type cell_type  = Rod | Cone of Color.t

type cell = { typ : cell_type
            ; x   : float
            ; y   : float
            }
 
type model = {
             state : state
             ; n : int
             ; retina : Image.t
             }
         
let imageOf c =
   match c with
   | Rod -> Image.circle (cellRadius) (veryDarkGray)
   | Cone(t) -> Image.circle (cellRadius) (t)
 
let newCellType () = 
   match Random.float(1.0) > 0.95 with
   | false -> Rod
   | true -> match Random.float(1.0) > 0.64 with
      | false -> Cone (Color.red)
      | true -> match Random.float(1.0) > 0.91666 with
         | true -> Cone (Color.blue)
         | false -> Cone (Color.green)
 
let rec placeRandomCell model =
   match model.state with
   | Ready -> model
   | Running ->
      let randX = Random.float(displayWidth) in
      let randY = Random.float(displayHeight) in
         match ((randX -. 400.0) ** 2.0 +. (randY -. 400.0) ** 2.0
         <= (radius -. 3.0) ** 2.0) with
         | false -> placeRandomCell (model)
         | true -> {model with
      retina = Image.placeImage (imageOf(newCellType()))
      (randX, randY) (model.retina)
      ; n = model.n - 1}
 
let finished model = model.n = 0
 
let mouse model x y event = match event with
   | "button_up" -> {model with retina = backing; state = Running}
   | _ -> model
 
let view model = model.retina
 
let initialModel = {state = Ready; n = 1000; retina = splash}

let go model =
   Animate.start model
      ~name: "Simple"
      ~rate: clockRate
      ~view: view
      ~stopWhen: finished
      ~width:displayWidth
      ~height:displayHeight
      ~onTick: placeRandomCell
      ~onMouse: mouse
      ~viewLast: view

let () = go initialModel