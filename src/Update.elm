module Update exposing (..)

import BasicsExtra exposing (..)
import NewTypes exposing (..)
import TType exposing (..)
import Model exposing (..)
import TTPicker

import Date exposing (Date, Unit(..))

import Array exposing (Array)
import Dict exposing (Dict)
import Tuple exposing (pair)

type Msg
  = TodayIs Date
  | PrevWeek
  | NextWeek
  | DragStartOn Die Index
  | MouseEnterCell Die Index
  | DragEnd
  | Clear Die Index
  | TTPickerMsg TTPicker.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    swap <| pair Cmd.none <| case msg of

        PrevWeek -> model |> changeDateBy -7 Days

        NextWeek -> model |> changeDateBy 7 Days

        TodayIs date -> model
            |> setToday (date |> toRataDie)
            |> setViewDate (Date.floor Date.Monday date)

        DragStartOn d i -> model
            |> setMouse
                ( if tsGet d i model.timesheet /= model.ttPicker.selection
                    then DrawingFrom i
                    else ErasingFrom i
                )
            |> tsMapFromModel (setAtIndexInDie i model.ttPicker.selection d)

        MouseEnterCell d i -> model |>
            let setter start tt = tsMapFromModel <| dieMap (setRange (minIndex start i) (maxIndex start i) tt) d
            in case model.mouseState of
                DrawingFrom start -> setter start model.ttPicker.selection
                ErasingFrom start -> setter start None
                NoDrag  -> identity

        DragEnd -> model |> setMouse NoDrag

        Clear d i -> model |> tsMapFromModel (dieMap (clear i None) d)

        TTPickerMsg ttMsg -> {model|ttPicker = model.ttPicker |> TTPicker.update ttMsg}

        NoOp -> model

------- LENSES

setViewDate x m =
    {m | viewingDate = x}
setToday x m =
    {m | today = x}
setMouse x m =
    {m | mouseState = x}

mapDate f m =
    {m | viewingDate = m.viewingDate |> f}
changeDateBy quantity unit =
    mapDate <| Date.add unit quantity

-------- TIMESHEET LENSES (SETTERS)

tsMapFromModel f m =
    {m | timesheet = m.timesheet |> f}

setAtIndexInDie : Index -> TType -> Die -> Timesheet -> Timesheet
setAtIndexInDie (Index i) tt =
    dieMap (Array.set i tt)

dieMap : (Array TType -> Array TType) -> Die -> Timesheet -> Timesheet
dieMap f (Die d) ts =
    ts |> Dict.insert d (ts |> getByDie (Die d) |> f)

setRange (Index start) (Index end) newTT =
    if start > end then identity else Array.set start newTT >> setRange (Index (start+1)) (Index end) newTT

clear (Index i) newTT arr =
    case Array.get i arr of
        Nothing -> arr
        Just iTT -> clearHelper iTT newTT (Index i) arr

clearHelper oldTT newTT (Index i) arr =
    case Array.get i arr of
        Nothing -> arr
        Just iTT ->
            if iTT /= oldTT then arr
            else Array.set i newTT arr |> clearHelper oldTT newTT (Index (i+1))