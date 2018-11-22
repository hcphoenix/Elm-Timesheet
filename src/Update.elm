module Update exposing (..)

import BasicsExtra exposing (..)
import Model exposing (..)

import Date exposing (Date, Unit(..))

import Dict
import Tuple exposing (pair)

type Msg
  = TodayIs Date
  | PrevWeek
  | NextWeek
  | DragStartOn DieMins
  | MouseEnterCell DieMins
  | DragEnd
  | SelectTT TType
  | WindowResize Int
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    swap <| pair Cmd.none <| case msg of

        PrevWeek -> model |> changeDateBy -7 Days

        NextWeek -> model |> changeDateBy 7 Days

        TodayIs date -> model
            |> setToday date
            |> setViewDate (Date.floor Date.Monday date)

        SelectTT tt -> model |> setTT tt

        DragStartOn dm ->
            setHoverCell Nothing <|
            if Dict.get dm model.timesheet == Just model.selectedTType then
                model
                |> mapTimesheet (Dict.remove dm)
                |> setDrag Erasing
            else
                model
                |> mapTimesheet (Dict.insert dm model.selectedTType)
                |> setDrag Writing

        MouseEnterCell dm -> model |>
            case model.dragState of
                Erasing -> mapTimesheet <| Dict.remove dm
                Writing -> mapTimesheet <| Dict.insert dm model.selectedTType
                NoDrag  -> setHoverCell <| Just dm

        DragEnd -> model |> setDrag NoDrag

        WindowResize w -> model |> setWW w

        NoOp -> model

-- LENSES

setTT x m =
    {m | selectedTType = x}
setWW x m =
    {m | windowWidth = x}
setViewDate x m =
    {m | viewingDate = x}
setToday x m =
    {m | todaysDate = x}
setDrag x m =
    {m | dragState = x}
setHoverCell x m =
    {m | hoveringCell = x}

mapTimesheet f m =
    {m | timesheet = m.timesheet |> f}
mapDate f m =
    {m | viewingDate = m.viewingDate |> f}

changeDateBy quantity unit =
    mapDate <| Date.add unit quantity