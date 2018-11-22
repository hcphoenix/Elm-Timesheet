import BasicsExtra exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (view)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events

import Date exposing (Date, Interval(..), Unit(..), toRataDie)
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
        , viewingDate = Date.fromCalendarDate 2018 Jan 1
        , todaysDate = Date.fromRataDie 0
        , dragState   = NoDrag
        , hoveringCell = Nothing
        , selectedTType = Work
        , ttPickerVisible = False
        , windowWidth = 640
        }
    , Cmd.batch
        [ Task.perform TodayIs Date.today
        , Task.perform (\vp -> WindowResize <| (round vp.scene.width)) Dom.getViewport
        , Task.perform (always NoOp) (Dom.setViewport 0 View.startingY)
        ]
    )

subs = Sub.batch
    [ Events.onResize (\w _ -> WindowResize w)
    , Events.onMouseUp <| succeed DragEnd
    --, Events.onKeyPress <| D.map keyToCmd <| D.field "key" D.string
    ]
{-
keyToCmd key =
    case key of
        "ArrowLeft" -> PrevWeek
        "ArrowRight" -> NextWeek
        _ -> NoOp
-}