module Model exposing (..)

import List exposing (..)
import Dict exposing (Dict)
import Date exposing (Date, toRataDie)

type alias Model =
  { timesheet : Dict DieMins TType
  , viewingDate : Date
  , todaysDate : Date
  , selectedTType : TType
  , ttPickerVisible : Bool
  , dragState   : DragState
  , hoveringCell : Maybe DieMins
  , windowWidth : Int
  }

type alias DieMins = (Int, Int)

type TType
  = Work
  | Sick
  | Vaca
allTTypes = [Work, Sick, Vaca]

type DragState
  = NoDrag
  | Writing
  | Erasing

minuteIncrements = 15

hoursOfTTInDate : TType -> Date -> Dict DieMins TType -> Float
hoursOfTTInDate tt date =
    Dict.filter ( \(die,_) v -> die == toRataDie date && v == tt
                )
    >> Dict.size >> toFloat >> (*) (minuteIncrements/60)