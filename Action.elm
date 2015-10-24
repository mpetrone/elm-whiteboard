module Action where

import Model exposing (..)
import Debug exposing (watch)

type Action = 
  StartDraw Point | Draw Point | EndDraw | ChangeColor PencilColor | ChangeWidth PencilWidth | ChangeMode PencilMode
update : Action -> Model -> Model
update action model = 
    case (Debug.watch "action" action) of
      StartDraw (x,y) -> 
        case List.head (List.map .points model.lines) of
          Nothing -> addLine [(x,y), (x + 0.1,y)] model
          Just [(x,y)] -> model
          Just list -> addLine [(x,y), (x + 0.1,y)] model

      EndDraw -> 
        case List.head (List.map .points model.lines) of
          Nothing -> model
          Just [] -> model
          Just list -> addLine []  {model | currentMode <- Free}

      Draw point -> 
        case List.head model.lines of
          Nothing -> model
          Just line -> if (line.points == []) 
                       then model 
                       else {model | lines  <- (addPoint line point) :: (Maybe.withDefault [] (List.tail model.lines)) }

      ChangeColor newColor -> {model | currentColor <- newColor}

      ChangeWidth newWidth -> {model | currentWidth <- newWidth}

      ChangeMode newMode -> {model | currentMode <- newMode}

addLine: List Point -> Model -> Model
addLine points' model =     
  { model | lines  <- 
    { color = model.currentColor
    , width = model.currentWidth
    , mode = model.currentMode
    , points = points'
    }
    :: model.lines }

addPoint: Line -> Point -> Line
addPoint line point =
  { line | points <- point :: line.points}