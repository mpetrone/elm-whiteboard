import Html exposing (Html)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Mouse exposing (Position)
import Window exposing (Size)
import Svg exposing (Svg)
import Svg.Attributes exposing (viewBox, fill, stroke, points, width, height)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type PencilColor = Red | Green | Blue | Black | White
type alias Line = { points: List Position, color: PencilColor }

type alias Model =     
    { lines : List Line,
      drawing : Bool,
      currentColor : PencilColor,
      windowSize : Size
    }


init : ( Model, Cmd Msg )
init =
  (Model [] False Black (Size 1279 704), Cmd.none)


-- UPDATE

type Msg = Movement Position | ClickHold Position | ClickRelease Position | Resize Size | ChangeColor PencilColor | Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg model =
  case msg of
    Movement position ->
      if model.drawing then
        { model | lines = addNewDraw model.lines position model.currentColor}   
      else 
        model  

    ClickHold position ->
      { model | 
        lines = (Line [] model.currentColor) :: model.lines,
        drawing = True
      }  

    ClickRelease position ->
      { model | drawing = False }  

    Resize size ->
      { model | windowSize = size }  

    ChangeColor color ->
      { model | currentColor = color }  

    Reset ->
      { model | lines = [] }  

addNewDraw: List Line -> Position -> PencilColor -> List Line
addNewDraw lines position color =
    case lines of
        head :: tail ->
            (Line (position :: head.points) color) :: tail
        [] -> 
            []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Mouse.moves (\position -> Movement position),
        Mouse.downs (\position -> ClickHold position),
        Mouse.ups (\position -> ClickRelease position),
        Window.resizes (\size -> Resize size)
    ]
 

-- VIEW

view : Model -> Html Msg
view model =
  let
    toolBoxHeight = 20
    heightString = toString model.windowSize.height
    widthString = toString model.windowSize.width
  in
    Html.div
      []
      [ buildToolBox toolBoxHeight
      , buildWhiteboard model widthString heightString
      ]

buildWhiteboard: Model -> String -> String -> Html Msg
buildWhiteboard model widthString heightString = 
  Svg.svg 
          [ width widthString
          , height heightString
          , viewBox ("0 0 " ++ widthString  ++ " " ++ heightString) 
          ]
          (List.map drawLine model.lines)

drawLine: Line -> Svg msg 
drawLine line =
    let 
        stringPoints =
            List.foldr (++) "" (List.map (\p -> (toString p.x) ++ "," ++ (toString p.y) ++ " ") line.points)
    in        
        Svg.polyline [ fill "none", stroke (toColorString line.color), points stringPoints ] []

buildToolBox: Int -> Html Msg
buildToolBox height =
  Html.div 
    [  
      style
        [ ("backgroundColor", "grey")
        , ("height", (toString height) ++ "px")
        , ("width", "100%")
        , ("position", "absolute")
        , ("z-index", "1000")
        ]
    ]
    [ buildButton (ChangeColor Red) "rojo"
    , buildButton (ChangeColor Green) "verde"
    , buildButton (ChangeColor Blue) "azul"
    , buildButton (ChangeColor Black) "negro"
    , buildButton Reset "borrar todo"
    ]

buildButton: Msg -> String -> Html Msg
buildButton msg buttonText =
  Html.button 
  [ onClick msg ] 
  [ Html.text buttonText ]

toColorString: PencilColor -> String
toColorString p =
  case p of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Black -> "black"
    White -> "white"
