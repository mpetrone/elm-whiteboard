module View where

import Json.Decode as Decode
import Html exposing (..)
import Html.Events exposing (on, onMouseDown, onMouseUp, onClick)
import Html.Attributes exposing (id, style, href)
import Model exposing (..)
import Debug exposing (watch)
import Action exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)

view : Signal.Address Action -> (Int, Int) -> Model -> Html
view address (w, h) model =
  div 
      [] 
      [ buildToolBoox address
      , buildBoard address (w, h) model
      , buildFooter
      ]


buildToolBoox: Signal.Address Action -> Html
buildToolBoox address =
  div 
    [ style [("position", "absolute")]
    ] 
    [ buildButton address Red (ChangeColor Red) 20
    , buildButton address Green (ChangeColor Green) 20
    , buildButton address Blue (ChangeColor Blue) 20
    , buildButton address Black (ChangeWidth Thin) 5
    , buildButton address Black (ChangeWidth Normal) 10
    , buildButton address Black (ChangeWidth Thick) 15
    , buildButton address White (ChangeMode Circle) 20
    ]

buildButton: Signal.Address Action -> PencilColor -> Action -> Float -> Html
buildButton address color action size=
    button 
    [ onClick address action
    , style [("position", "relative"), ("z-index", "1000")] ] 
    [ fromElement (collage 40 40 [filled (toColor color) (circle size)]) ]


buildBoard: Signal.Address Action -> (Int, Int) -> Model -> Html
buildBoard address (w, h) model =
  let parseLine line = if line.mode == Circle then parseCircle line else parseFreeLine line
      parseFreeLine line = traced (buildLineStyle line) (path line.points)
      parseCircle line = calculateRadius (w,h) line.points
                          |> circle 
                          |> outlined (buildLineStyle line)
                          |> move ( Maybe.withDefault (0,0) ( List.head (List.reverse (line.points))))
      decodeLocation = Decode.object2 (,) (Decode.at ["pageX"] Decode.float) (Decode.at ["pageY"] Decode.float)
  in div 
      [ id "my-div"
      , on "mousemove" decodeLocation (\(x,y)-> Signal.message address (Draw (toAbsolutPosition (x,y) (w,h))))
      , on "mousedown" decodeLocation (\(x,y)-> Signal.message address (StartDraw (toAbsolutPosition (x,y) (w,h))))
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

toAbsolutPosition: (Float, Float) -> (Int, Int) -> (Float, Float)
toAbsolutPosition (x,y) (w, h) =
    (x - toFloat w / 2, toFloat h / 2 - y)

buildLineStyle: Line -> LineStyle
buildLineStyle line = 
  { color = toColor line.color 
  , width = toWidth line.width 
  , cap   = Flat
  , join  = Sharp 10
  , dashing = []
  , dashOffset = 0
  } 

toWidth: PencilWidth -> Float
toWidth p = 
  case p of
    Thin -> 0.5
    Normal -> 1.5
    Thick -> 4  

toColor: PencilColor -> Color
toColor p =
  case p of
    Red -> rgb 200 0 0
    Green -> rgb 0 200 0
    Blue -> rgb 0 0 200
    Black -> rgb 0 0 0
    White -> rgb 255 255 255

calculateRadius: (Int, Int) -> List Point -> Float
calculateRadius dimensions points =
  let first = List.head points
      last = List.head (List.reverse points)
  in case (first, last) of 
    (Just f, Just l) -> 
      let (x1,y1) = toAbsolutPosition f dimensions
          (x2,y2) = toAbsolutPosition l dimensions
      in sqrt ((x1-x2)^2 + (y1-y2)^2)
    _ -> 0

buildFooter: Html
buildFooter =
   p 
    [ style [("position", "absolute")]]
    [ Html.text "Written by"
    , a [href "https://github.com/mpetrone"] [Html.text " Matias Petrone"]
    ]
