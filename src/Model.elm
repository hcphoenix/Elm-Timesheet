module Model exposing (..)

import TTPicker
import NewTypes exposing (..)
import BasicsExtra exposing (..)

import Dict.Any exposing (AnyDict)

import Date exposing (Date)
import Time exposing (Month(..))

type alias Model =
    { timesheet : Timesheet
    , viewingDate : Date
    , today : Date
    , hoverCol : Int
    , drawing : Maybe (Index,Index)
    , ttPicker : TTPicker.Model
    , recentShifts : List (Index,Index)
    , tooltip : String
    }

numTimesInDay = (24*60)//minuteIncrements - 1

abTestSheet = 
    let
        t = (*) (60//minuteIncrements) >> Index
        entry (s,e) = (t s, (prev (t e), Work))
        forApr day list = (Date.fromCalendarDate 2019 Apr day, List.map entry list |> Dict.Any.fromList (\(Index i) -> i))
        am = identity
        pm = modBy 12 >> (+) 12
        monwed =
            [ (am 8, pm 12)
            , (pm 1, pm 3)
            ]
        tuesthurs =
            [ (am 8, pm 4)
            ]
    in Dict.Any.fromList Date.toRataDie
        [ forApr 1 monwed
        , forApr 2 tuesthurs
        , forApr 3 monwed
        , forApr 4 tuesthurs
        ]