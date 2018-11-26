module Model exposing (..)

import TTPicker
import TType exposing (..)
import NewTypes exposing (..)
import BasicsExtra exposing (..)

import List exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Maybe exposing (withDefault)

import Date exposing (Date)

type alias Model =
  { timesheet : Timesheet
  , viewingDate : Date
  , today : Die
  , mouseState : MouseState
  , ttPicker : TTPicker.Model
  }

type alias Timesheet = Dict Int (Array TType)

type MouseState
  = NoDrag
  | DrawingFrom Index
  | ErasingFrom Index

------- LENSES (GETTERS)

numTimesInDay = (24*60)//15

getByDie : Die -> Timesheet -> Array TType
getByDie (Die d) =
    Dict.get d >> withDefault (Array.repeat numTimesInDay None)

tsGet : Die -> Index -> Timesheet -> TType
tsGet (Die d) (Index i) =
    Dict.get d >> Maybe.map (Array.get i >> withDefault None) >> withDefault None