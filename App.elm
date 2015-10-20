module App where

import Html exposing (Html)
import Signal exposing (Address)
import Board exposing (initModel, view, update)
import Window

type alias Config model action =
    { model : model
    , view : Address action -> (Int, Int) -> model -> Html
    , update : action -> model -> model
    }

main =
  start 
    { model = initModel
    , view = view
    , update = update 
    }    

start : Config model action -> Signal Html
start config =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    model =
      Signal.foldp
        (\(Just action) model -> config.update action model)
        config.model
        actions.signal
  in
    Signal.map2 (config.view address) Window.dimensions model
