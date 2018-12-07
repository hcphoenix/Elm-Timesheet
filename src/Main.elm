import BasicsExtra exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (view)
import NewTypes exposing (..)
import TTPicker

import Browser
import Browser.Dom as Dom
import Browser.Events as Events

import Date exposing (Date, Interval(..), Unit(..))
import Time exposing (Month(..))

import Dict
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
    (   { timesheet = Dict.empty
        , viewingDie = toRataDie <| Date.fromCalendarDate 2018 Jan 1
        , today = Die 0
        , drawState = Nothing
        , ttPicker = TTPicker.init
        , tooltip = ""
        }
    , Cmd.batch
        [ Task.perform TodayIs Date.today
        , Task.perform (always NoOp) (Dom.setViewport 0 View.startingY)
        ]
    )

subs = Sub.batch
    [ Events.onMouseUp <| succeed DrawEnd
    , Events.onKeyPress <| D.map keyToCmd <| D.field "key" D.string
    ]

keyToCmd key =
    case key of
        "ArrowLeft" -> PrevWeek
        "ArrowRight" -> NextWeek
        _ -> NoOp