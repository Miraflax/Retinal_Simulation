let displayWidth = 800.
let displayHeight = displayWidth
let radius = displayWidth /. 2.0
let cellRadius = 3.0
let clockRate = 0.01
let veryDarkGray = Color.makeColor 64 64 64
let backing = Image.circle radius Color.white
let text = Image.text "Click" veryDarkGray ~size:50.0
let splash  = Image.placeImage text (radius -. 50., radius -. 20.) backing
 
type state = Ready | Running
 
(* Colors are Red, Green and Blue.
*)
type cell_type  = Rod | Cone of Color.t

type cell = { typ : cell_type
            ; x   : float
            ; y   : float
            }
 
type model = {
             state : state
             ; n : int
             ; retina : Image.t
             ; placedCells : cell list
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

let newCell cellType =
   let rec loop () =
      let randX = Random.float(displayWidth) in
      let randY = Random.float(displayHeight) in
      match cellType with
      | Rod -> (match ((randX -. 400.0) ** 2.0 +. (randY -. 400.0) ** 2.0 <= 
      (radius -. 1.5) ** 2.0) with
         | false -> loop()
         | true -> {typ = cellType; x = randX; y = randY})
         
      | Cone(t)-> ((match ((randX -. 400.0) ** 2.0 +. (randY -. 400.0) ** 2.0
       <= (radius /. 5.0) ** 2.0) with
         | false -> loop()
         | true -> {typ = cellType; x = randX; y = randY}))
in
loop()
 
 
let finished model = model.n = 0
 
let mouse model x y event = match event with
| "button_up" -> {model with retina = backing; state=Running}
| _ -> model
 
let view model = model.retina
 
 
let cellDistance a b = sqrt((a.x -. b.x) ** 2.0 +. (a.y -. b.y) ** 2.0)

let rec getShortestDistance placedCells testCell shortestDistance = 
   match placedCells with
   | [] -> shortestDistance
   | placedCell :: placedCells ->
    ((let distance = (cellDistance placedCell testCell) in
      match distance < shortestDistance with
      | true -> getShortestDistance placedCells testCell distance
      | false -> getShortestDistance placedCells testCell shortestDistance)
)
 
let rec generateRandomCells n cellType= match n > 0 with
   | true -> newCell(cellType) :: generateRandomCells(n-1) (cellType)
   | false -> []
 
let basicCell = {typ=newCellType(); x = 20.0; y = 42.0}
 
let bestAlgorithm model =
   let rec loop placedCells testCells bestCell greatestDistance =
      match testCells with
      | [] -> bestCell
      | cell :: cells -> (
         let shortestDistance =
            (getShortestDistance placedCells cell (2.0 *. radius)) in
         match shortestDistance >= greatestDistance with
         | true -> loop placedCells cells cell shortestDistance
         | false -> loop placedCells cells bestCell greatestDistance
         )
      in
   loop (model.placedCells) (generateRandomCells (20) (newCellType())) basicCell (-1.0)
 
   let rec placeBestCell model =
      match model.state with
      | Ready -> model
      | Running -> let newCell = bestAlgorithm model in
         {
         state = model.state
         ; retina = Image.placeImage (imageOf(newCell.typ))
            (newCell.x, newCell.y) (model.retina)
         ; n = model.n - 1
         ; placedCells = newCell :: model.placedCells
         }
 
let go model =
   Animate.start model
      ~name: "Fovea"
      ~rate: clockRate
      ~view: view
      ~stopWhen: finished
      ~width:displayWidth
      ~height:displayHeight
      ~onTick: placeBestCell
      ~onMouse: mouse
      ~viewLast: view
 
let initialModel = {state = Ready; n = 1000; retina = splash; placedCells = []}
 
let _ = go initialModel