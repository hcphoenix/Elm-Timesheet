module Update exposing (..)

import BasicsExtra exposing (..)
import NewTypes exposing (..)
import TType exposing (..)
import Model exposing (..)
import TTPicker

import Date exposing (Date, Unit(..))

import Dict exposing (Dict, get, insert, remove, empty)
import Tuple exposing (pair)
import Maybe exposing (withDefault)

type Msg
  = TodayIs Date
  | PrevWeek
  | NextWeek
  | DrawCell Int Index
  | DrawEnd
  | XBlock Index Int
  | TTPickerMsg TTPicker.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        ts = model.timesheet
        vd = model.viewingDie
        tsMap f (Die d) = insert d (f <| withDefault empty <| get d ts) ts
        shift n = Die (dieToInt vd |> (+) n)
    in swap <| pair Cmd.none <| case msg of

        PrevWeek ->
            { model|viewingDie = shift -7}

        NextWeek ->
            { model|viewingDie = shift 7 }

        TodayIs date ->
            { model|viewingDie = toRataDie <| Date.floor Date.Monday date }

        DrawCell o i ->
            { model
            | drawState = case model.drawState of
                Just (_,start,_) -> Just (o,start,i)
                Nothing -> Just (o,i,i)
            }

        DrawEnd ->
            { model
            | timesheet = case model.drawState of
                Nothing -> ts
                Just (offset,(Index start),(Index end)) ->
                    let
                        i = min start end
                        len = max start end - i + 1
                    in tsMap (insert i (len, model.ttPicker.selection)) <| (shift offset)
            , drawState = Nothing
            }

        XBlock (Index i) offset ->
            { model|timesheet = tsMap (remove i) <| shift offset }

        TTPickerMsg ttMsg ->
            { model|ttPicker = model.ttPicker |> TTPicker.update ttMsg }

        NoOp -> model