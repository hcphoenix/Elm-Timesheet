module Model exposing (..)

import TTPicker
import TType exposing (..)
import NewTypes exposing (..)

import Dict exposing (Dict)

import Date exposing (Date)

type alias Model =
  { timesheet : Timesheet
  , viewingDie : Die
  , today : Die
  , drawState : Maybe (Int,Index,Index)
  , ttPicker : TTPicker.Model
  , tooltip : String
  }

type alias Timesheet = Dict Int (Dict Int (Int, TType))

numTimesInDay = (24*60)//minuteIncrements