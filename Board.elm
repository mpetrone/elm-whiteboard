import Json.Decode as Decode
import Html exposing (..)
import Html.Events exposing (on, onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (id, style)
import App
import Color exposing (..)
import Graphics.Collage exposing (..)
import Debug

main =
  App.start { model = initModel, view = view, update = update }

-- MODEL

type alias Point = (Float, Float)
type alias Line = { points: List Point, color: PencilColor }
type alias Model = { currentColor: PencilColor, lines: List Line }
type PencilColor = Red | Green | Blue
initModel: Model
initModel = { currentColor = Red, lines = [] }

-- ACTION

type Action = StartDraw Point | Draw Point | EndDraw | Change PencilColor
update : Action -> Model -> Model
update action model = 
    case (Debug.watch "action" action) of
      StartDraw (x,y) -> 
        case List.head (List.map .points model.lines) of
          Nothing -> addLine [(x,y), (x + 0.5,y)] model
          Just [(x,y)] -> model
          Just list -> addLine [(x,y), (x + 0.5,y)] model

      EndDraw -> 
        case List.head (List.map .points model.lines) of
          Nothing -> model
          Just [] -> model
          Just list -> addLine [] model

      Draw point -> 
        case List.head model.lines of
          Nothing -> model
          Just line -> if (line.points == []) 
                       then model 
                       else {model | lines  <- (addPoint line point) :: (Maybe.withDefault [] (List.tail model.lines)) }

      Change newColor -> {model | currentColor <- newColor}

addLine: List Point -> Model -> Model
addLine points' model =     
  { model | lines  <- {color = model.currentColor, points = points'} :: model.lines }

addPoint: Line -> Point -> Line
addPoint line point =
  { line | points <- point :: line.points}

-- VIEW

view : Signal.Address Action -> (Int, Int) -> Model -> Html
view address (w, h) model =
  div 
      [] 
      [ div [style [("position", "absolute")]] 
        [buildButton address Red
        , buildButton address Green
        , buildButton address Blue
        ]
      , buildBoard address (w, h) model
      ]


buildButton: Signal.Address Action -> PencilColor -> Html
buildButton address color =
    button 
    [ onClick address (Change color) 
    , style [("position", "relative"), ("z-index", "1000")] ] 
    [ fromElement (collage 40 40 [filled (toColor color) (circle (toFloat 20))]) ]


buildBoard: Signal.Address Action -> (Int, Int) -> Model -> Html
buildBoard address (w, h) model =
  let parseLine line = traced (solid (toColor line.color)) (path line.points)
      decodeLocation = Decode.object2 (,) (Decode.at ["pageX"] Decode.float) (Decode.at ["pageY"] Decode.float)
  in div 
      [ id "my-div"
      , on "mousemove" decodeLocation (\(x,y)-> Signal.message address (Draw (x - toFloat w / 2, toFloat h / 2 - y)))
      , on "mousedown" decodeLocation (\(x,y)-> Signal.message address (StartDraw (x - toFloat w / 2, toFloat h / 2 - y)))
      , onMouseUp address EndDraw
      ]
      [  
        model.lines
            |> Debug.watch "lines"
            |> List.reverse
            |> List.map parseLine
            |> collage w h 
            |> fromElement
      ]

toColor: PencilColor -> Color
toColor p =
  case p of
    Red -> rgb 200 0 0
    Green -> rgb 0 200 0
    Blue -> rgb 0 0 200
 
      