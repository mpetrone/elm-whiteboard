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
import Debug

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

type PencilWidth = 
    Thin 
  | Normal 
  | Thick 

type alias Line = 
  { points: List Position
  , color: PencilColor 
  , width: PencilWidth }

type alias Model =     
  { lines : List Line
  , drawing : Bool
  , currentColor : PencilColor
  , currentWidth : PencilWidth
  , windowSize : Size
  , input: String
  , host: Maybe String
  }


init : ( Model, Cmd Msg )
init =
  (Model [] False Black Normal (Size 1279 704) "" Nothing, Cmd.none)

-- UPDATE

type Msg = 
    Draw Position
  | WebSocketDraw Position
  | ClickHold Position 
  | ClickRelease Position 
  | Resize Size 
  | ChangeColor PencilColor 
  | ChangeWidth PencilWidth
  | ResetWhiteboad
  | Input String
  | Connect
  | NewMessage (List Line)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Draw position ->
      ({ model | lines = addNewDraw model.lines position model.currentColor model.currentWidth}, 
        case model.host of
          Just host -> WebSocket.send host (Debug.log "sent" (Encode.encode 0 (encodePosition position)))
          Nothing -> Cmd.none)

    WebSocketDraw position ->
      ({ model | lines = addNewDraw model.lines position model.currentColor model.currentWidth}, Cmd.none)  

    ClickHold position ->
      ({ model | 
        lines = (Line [] model.currentColor model.currentWidth) :: model.lines,
        drawing = True
      } , Cmd.none) 

    ClickRelease position ->
      ({ model | drawing = False }, Cmd.none)

    Resize size ->
      ({ model | windowSize = size }, Cmd.none)  

    ChangeColor color ->
      ({ model | currentColor = color }, Cmd.none) 

    ChangeWidth width ->
      ({ model | currentWidth = width }, Cmd.none) 

    ResetWhiteboad ->
      ({ model | lines = [] }, Cmd.none)   

    Input newInput ->
      ({ model | input = newInput }, Cmd.none)   

    Connect ->
      ({ model | host = Just model.input }, Cmd.none) 

    NewMessage lines ->
      ({ model | lines = lines }, Cmd.none)    

addNewDraw: List Line -> Position -> PencilColor -> PencilWidth -> List Line
addNewDraw lines position color width =
    case lines of
        head :: tail ->
            (Line (position :: head.points) color width) :: tail
        [] -> 
            []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        (if model.drawing then Mouse.moves (\position -> Draw position)
                          else Sub.none)
        ,Mouse.downs (\position -> ClickHold position)
        ,Mouse.ups (\position -> ClickRelease position)
        ,Window.resizes (\size -> Resize size),
        case model.host of
          Just host -> WebSocket.listen host decodePositionToMsg
          Nothing -> Sub.none
    ]
 
decodePositionToMsg: String -> Msg 
decodePositionToMsg json = 
  case Decode.decodeString decodePosition json of
    Ok position -> Debug.log "received" (WebSocketDraw position)
    Err error -> Debug.crash error

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
    [ style
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
    , buildButton (ChangeWidth Thin) "fino"
    , buildButton (ChangeWidth Normal) "normal"
    , buildButton (ChangeWidth Thick) "grueso"
    , buildButton ResetWhiteboad "borrar todo"
    , Html.input [onInput Input] []
    , buildButton Connect "Connect"
    ]

buildButton: Msg -> String -> Html Msg
buildButton msg buttonText =
  Html.button 
  [ onClick msg ] 
  [ Html.text buttonText ]

-- JSON


decodePosition : Decode.Decoder Position
decodePosition =
    Decode.object2 Position
      ("x" := Decode.int)
      ("y" := Decode.int)

decodePencilColor : String -> Decode.Decoder PencilColor
decodePencilColor color = Decode.succeed (toPencilColor color)

decodePencilWidth : String -> Decode.Decoder PencilWidth
decodePencilWidth width = Decode.succeed (toPencilWidth width)

encodePosition : Position -> Encode.Value
encodePosition record =
    Encode.object
        [ ("x", Encode.int record.x)
        , ("y", Encode.int record.y)
        ]        

encodePencilColor : PencilColor -> Encode.Value
encodePencilColor color = Encode.string (toColorString color)

encodePencilWidth : PencilWidth -> Encode.Value
encodePencilWidth width = Encode.string (toWidthString width)

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

toPencilWidth : String -> PencilWidth
toPencilWidth width =
  case width of
    "thin" -> Thin
    "normal" -> Normal
    "thick" -> Thick
    _ -> Normal
    
toWidthString: PencilWidth -> String
toWidthString p =
  case p of
    Thin -> "thin"
    Normal -> "normal"
    Thick -> "thick"

