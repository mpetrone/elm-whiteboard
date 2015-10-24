module Model where

type PencilColor = Red | Green | Blue | Black | White
type PencilWidth = Thin | Normal | Thick
type PencilMode = Circle | Free

type alias Point = (Float, Float)
type alias Line = { points: List Point, color: PencilColor, width: PencilWidth, mode: PencilMode }
type alias Model = { currentMode: PencilMode, currentColor: PencilColor, currentWidth: PencilWidth, lines: List Line }

initModel: Model
initModel = { currentMode = Free, currentColor = Red, currentWidth = Normal, lines = [] }
