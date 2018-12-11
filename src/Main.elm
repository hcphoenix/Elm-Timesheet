import BasicsExtra exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (view)
import NewTypes exposing (..)
import TTPicker

import Browser
import Browser.Dom as Dom
import Browser.Events as Events

import Date exposing (Date, Interval(..), Unit(..), toRataDie)
import Time exposing (Month(..))

import Dict.Any exposing (AnyDict)
import Task
import Json.Decode as D exposing (succeed)

import Debug

main : Program () Model Msg
main = Browser.element
      { init = always init
      , view = view
      , update = update
      , subscriptions = always subs
      }

init : ( Model, Cmd Msg )
init =
    (   { timesheet = abTestSheet --Dict.Any.empty toRataDie
        , viewingDate = Date.floor Date.Monday <| Date.fromCalendarDate 2019 Feb 1
        , today = Date.fromRataDie 9
        , drawing = Nothing
        , hoverCol = 0
        , ttPicker = TTPicker.init
        , recentShifts = []
        , tooltip = ""
        }
    , Cmd.batch
        [ Task.perform (always NoOp) (Dom.setViewport 0 View.startingY)
        --, Task.perform TodayIs Date.today
        ]
    )

subs = Sub.batch
    [ Events.onKeyPress <| D.map keyToCmd <| D.field "key" D.string
    --, Events.onMouseUp <| succeed DrawEnd
    ]

keyToCmd key =
    case key of
        "ArrowLeft" -> PrevWeek
        "ArrowRight" -> NextWeek
        _ -> NoOp