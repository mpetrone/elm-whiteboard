import Html exposing (Html)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Mouse exposing (Position)
import Window exposing (Size)
import Svg exposing (Svg)
import Svg.Attributes exposing (viewBox, fill, stroke, points, width, height)
import WebSocket
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode exposing ((:=))

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type PencilColor = 
    Red 
  | Green 
  | Blue 
  | Black 
  | White 

type alias Line = 
  { points: List Position
  , color: PencilColor }

type alias Model =     
  { lines : List Line
  , drawing : Bool
  , currentColor : PencilColor
  , windowSize : Size
  , input: String
  , listenHost: Maybe String
  }


init : ( Model, Cmd Msg )
init =
  (Model [] False Black (Size 1279 704) "" Nothing, Cmd.none)


-- UPDATE

type Msg = 
    Movement Position 
  | ClickHold Position 
  | ClickRelease Position 
  | Resize Size 
  | ChangeColor PencilColor 
  | ResetWhiteboad
  | Input String
  | Send
  | Listen
  | NewMessage (List Line)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Movement position ->
      if model.drawing then
        ({ model | lines = addNewDraw model.lines position model.currentColor}, Cmd.none)
      else 
        (model, Cmd.none)

    ClickHold position ->
      ({ model | 
        lines = (Line [] model.currentColor) :: model.lines,
        drawing = True
      } , Cmd.none) 

    ClickRelease position ->
      ({ model | drawing = False }, Cmd.none)

    Resize size ->
      ({ model | windowSize = size }, Cmd.none)  

    ChangeColor color ->
      ({ model | currentColor = color }, Cmd.none) 

    ResetWhiteboad ->
      ({ model | lines = [] }, Cmd.none)   

    Input newInput ->
      ({ model | input = newInput }, Cmd.none)   

    Send ->
      ( model , 
        WebSocket.send model.input 
        (Encode.encode 0 
          (Encode.list 
            (List.map encodeLine model.lines)
          )
        )
      )

    Listen ->
      ({ model | listenHost = Just model.input }, Cmd.none) 

    NewMessage lines ->
      ({ model | lines = lines }, Cmd.none)   

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
        Window.resizes (\size -> Resize size),
        case model.listenHost of
          Just host -> WebSocket.listen host decodeLineList
          Nothing -> Sub.none
    ]
 
decodeLineList: String -> Msg 
decodeLineList json = 
    let decoder = Decode.list decodeLine
    in 
      case Decode.decodeString decoder json of
        Ok lines -> NewMessage lines
        Err error -> NewMessage []

-- JSON

decodeLine : Decode.Decoder Line
decodeLine =
    Decode.object2 Line
      ("points" := Decode.list decodePosition)
      (("color" := Decode.string) `Decode.andThen` decodePencilColor)

decodePosition : Decode.Decoder Position
decodePosition =
    Decode.object2 Position
      ("x" := Decode.int)
      ("y" := Decode.int)

decodePencilColor : String -> Decode.Decoder PencilColor
decodePencilColor color = Decode.succeed (toPencilColor color)

encodeLine : Line -> Encode.Value
encodeLine record =
    Encode.object
        [ ("points",  Encode.list (List.map encodePosition  record.points))
        , ("color",  encodePencilColor record.color)
        ]

encodePosition : Position -> Encode.Value
encodePosition record =
    Encode.object
        [ ("x",   Encode.int record.x)
        , ("y",  Encode.int  record.y)
        ]        

encodePencilColor : PencilColor -> Encode.Value
encodePencilColor color = Encode.string (toColorString color)

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
    , buildButton ResetWhiteboad "borrar todo"
    , Html.input [onInput Input] []
    , buildButton Send "Send"
    , buildButton Listen "Listen"
    ]

buildButton: Msg -> String -> Html Msg
buildButton msg buttonText =
  Html.button 
  [ onClick msg ] 
  [ Html.text buttonText ]


-- AUX

toPencilColor : String -> PencilColor
toPencilColor color =
  case color of
    "red" -> Red
    "blue" -> Blue
    "green" -> Green
    "black" -> Black
    "white" -> White
    _ -> Black
    
toColorString: PencilColor -> String
toColorString p =
  case p of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    Black -> "black"
    White -> "white" 
