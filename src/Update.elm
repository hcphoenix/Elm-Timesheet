module Update exposing (..)

import BasicsExtra exposing (..)
import NewTypes exposing (..)
import Model exposing (..)
import TTPicker

import Date exposing (Date, Unit(..))

import List exposing (append, take)
import List.Extra exposing (uniqueBy)
import Dict.Any exposing (AnyDict, get, insert, remove, empty)
import Tuple exposing (pair)
import Maybe exposing (withDefault)

type Msg
  = TodayIs Date
  | PrevWeek
  | NextWeek
  | EnterCol Int
  | DrawCell Index
  | DrawEnd
  | XBlock Index Int
  | TTPickerMsg TTPicker.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        ts = model.timesheet
        vd = model.viewingDate
        tsMap f d = insert d (f <| tsGet d ts) ts
        shift n = Date.add Days n model.viewingDate
    in swap <| pair Cmd.none <| case msg of

        PrevWeek ->
            { model|viewingDate = shift -7}

        NextWeek ->
            { model|viewingDate = shift 7 }

        TodayIs date ->
            { model|viewingDate = Date.floor Date.Monday date }

        EnterCol i ->
            { model|hoverCol = i}

        DrawCell i ->
            { model|drawing = case model.drawing of 
                Nothing -> Just (i,i)
                Just (start,_) -> Just (start,i)
            }
            
        DrawEnd ->
            { model
            | drawing = Nothing
            , timesheet = case model.drawing of
                Nothing -> ts --Should be unreachable, consider refactor
                Just (a,b) ->
                    tsMap (insert (minIndex a b) ((maxIndex a b), model.ttPicker.selection)) <| shift model.hoverCol
            , recentShifts = case model.drawing of
                Nothing -> model.recentShifts --Should be unreachable, consider refactor
                Just x ->
                    x :: model.recentShifts |> uniqueBy (\(Index s,Index e) -> (s,e)) |> take 3 
            }

        XBlock index offset ->
            { model|timesheet = shift offset |> tsMap (remove index) }

        TTPickerMsg ttMsg ->
            { model|ttPicker = model.ttPicker |> TTPicker.update ttMsg }

        NoOp -> model