module View exposing (view, startingY)

import BasicsExtra exposing (..)
import Model exposing (..)
import TType exposing (..)
import NewTypes exposing (..)
import Update exposing (..)
import ViewUtils exposing (..)
import TTPicker

import Html exposing (Html)
import Html.Attributes as Attr

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input exposing (button)
import Element.Events exposing (onMouseDown, onMouseEnter, onMouseLeave)
import Element.Lazy exposing (..)
import Element.Keyed as Keyed

import Date exposing (Date, Interval(..), Unit(..), fromRataDie)

import List exposing (map, map2, indexedMap, range, repeat, singleton, sum, length, filter)
import List.Extra exposing (groupWhile)
import Dict exposing (Dict)
import Array exposing (Array)
import Tuple exposing (first, second, pair, mapFirst, mapSecond)
import Maybe exposing (withDefault)

view : Model -> Html Msg
view model =
  let
    mapDays f = map f <| daysInView model.viewingDate
  in layout globalStyle <| row
        [ inFront <| lazy4 header model.ttPicker model.viewingDate model.today model.timesheet
        , behindContent timeLabels
        , cssStyle "-moz-user-select" "none"
        ]
        <| map (timeColumn model) <| diesInView model.viewingDate

globalStyle =
    [ Font.center
    , Font.family [ Font.sansSerif ]
    , Font.size 12
    ]

-- TIMES

daysInView : Date -> List Date
daysInView viewingDate = Date.range Day 1 viewingDate (Date.add Days 7 viewingDate)

diesInView : Date -> List Die
diesInView = daysInView >> map toRataDie

printMins : Mins -> String
printMins (Mins time) =
  let
    hour = time // 60 |> modBy 12 |> (\h -> if h == 0 then 12 else h) |> String.fromInt
    mins = if time |> divisibleBy 60
                then ""
                else time |> modBy 60 |> String.fromInt |> String.padLeft 2 '0' |> (++) ":"
    ampm = if time < 12*60 then "AM" else "PM"
  in
    hour ++ mins ++ " " ++ ampm

printDieWhereTodayIs : Die -> Die -> String
printDieWhereTodayIs (Die today) (Die d) =
   case d - today of
        1  -> "Tomorrow"
        0  -> "Today"
    -- -1  -> "Yesterday" --Elm parser bug???
        _  -> Date.format "EEE d" (fromRataDie d)

-- TEMP ELEMENTS

timeLabels =
    column
        [ width fill
        , moveDown headerHeight
        , spacing cellHeight
        , padding cellHeight
        ]
        ( stride {from=0, to=24*60-1, by=60} |> map
            ( Mins >> printMins >> text >> el
                [ width fill
                , height <| px <| cellHeight * (45//minuteIncrements)
                , Font.size 40
                , Font.center
                , Font.color <| gray 0.8
                ]
            )
        )

-- TIMESHEET

timeColumn : Model -> Die -> Element Msg
timeColumn model die =
    let lst = getByDie die model.timesheet |> Array.toIndexedList |> map (mapFirst Index)
    in el
        ( [ moveDown <| toFloat headerHeight
          ] ++ (map inFront <| ttBlocks lst)
        ) <| lazy4 cells model.mouseState model.today model.ttPicker.selection die

ttBlocks : List (Index, TType) -> List (Element Msg)
ttBlocks =
    countGroupsBy (eqBy second) >> filter (first >> second >> (/=) None) >> map
    (\((i, tt), count) -> lazy3 ttBlock i count tt)

ttBlock : Index -> Int -> TType -> Element Msg
ttBlock (Index index) count tt =
    el
        [ colWidths
        , height <| px <| cellHeight * count
        , moveDown <| toFloat <| cellHeight * index
        , clipX
        , clipY
        , Border.solid
        , Border.width 1
        , Border.color <| gray 0.3
        , Background.color <| colorFromTType tt
        , Font.size 16
        , cssStyle "pointer-events" "none"
        ]
    <| text <| joinBy '\n'
        [ printTType tt
        , printMins (index |> Index |> toMins) ++ " - " ++ printMins (index+count |> Index |> toMins)
        ]

cells : MouseState -> Die -> TType -> Die -> Element Msg
cells mouse today sTT die =
    iRange 0 numTimesInDay |> map (cell mouse today sTT die)
    |> column
        [ Border.solid
        , Border.width 1
        , Border.color <| gray 0.6
        ]

cell : MouseState -> Die -> TType -> Die -> Index -> Element Msg
cell mouse today sTT die index =
    let mins = index |> toMins
    in el
        [ if mouse == NoDrag
            then onMouseDown <| DragStartOn die index
            else onMouseEnter <| MouseEnterCell die index
        , colWidths
        , height <| px cellHeight
        , pointer
        , Border.solid
        , Border.widthEach
            { left   = 0
            , right  = 0
            , bottom = 0
            , top    = if mins |> isHour then 2 else 1
            }
        , Border.color <| gray <| if mins |> isHour then 0.4 else 0.8
        , Font.color invisible
        , Background.color <| invisible
        , mouseOver
            [ Background.color <| colorFromTTypeWithFade 0.9 sTT
            , Font.color <| gray 0.7
            ]
        ]
        <| text <| printMins mins ++ " " ++ (die |> printDieWhereTodayIs today)

-- HEADER

header : TTPicker.Model -> Date -> Die -> Timesheet -> Element Msg
header ttPicker vDate today timesheet =
    column headerStyle
    [ row
        [ centerX
        , spacing 10
        , width fill
        , inFront <| Element.map TTPickerMsg <| TTPicker.view ttPicker timesheet vDate
        ]
        [ prevButton
        , currentDateDisplay vDate
        , nextButton
        ]
    , vDate |> dateHeadersWhereTodayIs today
    ]

currentDateDisplay vDate = vDate |> el currentDateStyle << text << Date.format " MMM y "

prevButton = button buttonStyle {onPress = Just <| PrevWeek, label = text "← Prev Week"}
nextButton = button buttonStyle {onPress = Just <| NextWeek, label = text "Next Week →"}

dateHeadersWhereTodayIs today =
    diesInView >> map
        ( \die -> printDieWhereTodayIs today die |> text
            |> el (thStyle ++ if die == today then [Font.bold] else [])
        )
    >> row [centerX, width fill]

headerStyle =
    headerBorder ++
    [ Font.size 18
    , Background.color white
    , vw 100
    , height <| px headerHeight
    , spacing 10
    , padding 5
    , cssStyle "position" "fixed"
    , cssStyle "top" "0"
    , Region.description "test"
    ]

headerBorder =
    [ Border.solid
    , Border.color <| gray 0.6
    , Border.widthEach
        { top = 0
        , left = 0
        , right = 0
        , bottom = 1
        }
    ]

currentDateStyle =
    [ Font.size 32
    ]

buttonStyle =
    [ centerX
    , vw 12
    , vh 5
    , Border.solid
    , Border.width 1
    , Border.color <| gray 0.8
    ]

thStyle =
    [ colWidths
    , centerX
    ]

-- SIZES

startingY = 9 * (60 / minuteIncrements) * cellHeight

headerHeight = 72

cellHeight = 15

colWidths = vw 14