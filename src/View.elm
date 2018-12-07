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

import Date exposing (Date, fromRataDie)

import List exposing (map, map2, indexedMap, range, repeat, singleton, sum, length, filter)
import Dict exposing (Dict, get, empty)
import Tuple exposing (first, second, pair, mapFirst, mapSecond)
import Maybe exposing (withDefault)

view : Model -> Html Msg
view model =
  let
    mapDies f = map f <| diesInView model.viewingDie
  in layout globalStyle <| row
            [ inFront <| header model.ttPicker model.viewingDie model.today model.timesheet
            , behindContent timeLabels
            , cssStyle "-moz-user-select" "none"
            ] <| indexedMap (timeColumn model) <| diesInView model.viewingDie

globalStyle =
    [ Font.center
    , Font.family [ Font.sansSerif ]
    , Font.size 12
    ]

-- TIMES

diesInView : Die -> List Die
diesInView (Die d) = range d (d+6) |> map Die

printMins : Mins -> String
printMins (Mins time) =
    let
        hour = time // 60 |> modBy 12 |> (\h -> if h == 0 then 12 else h) |> String.fromInt
        mins = if time |> divisibleBy 60
                    then ""
                    else time |> modBy 60 |> String.fromInt |> String.padLeft 2 '0' |> (++) ":"
        ampm = if time < 12*60 then "AM" else "PM"
    in hour ++ mins ++ " " ++ ampm

printDieWhereTodayIs : Die -> Die -> String
printDieWhereTodayIs (Die today) (Die d) =
   case d - today of
        1  -> "Tomorrow"
        0  -> "Today"
    -- -1  -> "Yesterday" --Elm parser bug???
        _  -> Date.format "EEE d" (fromRataDie d)

-- VARIOUS

paycheckEstimate ts vd =
    let total = diesInView vd |> mapcat
            (\d -> get (dieToInt d) ts |> withDefault empty |> Dict.toList |> map (second >> first)
            ) |> sum |> toFloat |> (*) (12.5 * minuteIncrements/60) |> round
    in el [] <| if total == 0 then Element.none else 
        text <| (++) "Estimated pay for this week: $" <| String.fromInt total

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

timeColumn : Model -> Int -> Die -> Element Msg
timeColumn model offset (Die d) =
    let lst = get d model.timesheet |> withDefault empty |> Dict.toList
    in el
        ( [ moveDown <| toFloat headerHeight
          ]
        ++ (map inFront <| map (ttBlock offset) lst)
        ++ case model.drawState of
            Nothing -> []
            Just (o,Index start,Index end) ->
                if o==offset then
                    [inFront <| ttBlock o ((min start end),(max start end - min start end + 1, model.ttPicker.selection)) ]
                else []
        ) <| lazy3 cells model.drawState model.ttPicker.selection offset

ttBlock : Int -> (Int, (Int, TType)) -> Element Msg
ttBlock offset (start, (len,tt)) =
    el
        [ colWidths
        , height <| px <| cellHeight * len
        , moveDown <| toFloat <| cellHeight * start
        , clipX
        , clipY
        , Border.solid
        , Border.width 1
        , Border.color <| gray 0.3
        , Background.color <| colorFromTType tt
        , Font.size 16
        --, cssStyle "pointer-events" "none"
        , inFront <| el
            [ alignRight
            , Font.bold
            , onMouseDown <| XBlock (Index start) offset
            , pointer
            ]
            <| text "X"
        ]
    <| text <| joinBy '\n'
        [ printTType tt
        , printMins (Index start |> toMins) ++ " - " ++ printMins (Index (start+len) |> toMins)
        ]

cells : Maybe (Int,Index,Index) -> TType -> Int -> Element Msg
cells mouse sTT offset =
    iRange 0 numTimesInDay |> map (cell mouse sTT offset)
    |> column
        [ Border.solid
        , Border.width 1
        , Border.color <| gray 0.6
        ]

cell : Maybe (Int,Index,Index) -> TType -> Int -> Index -> Element Msg
cell mouse sTT offset index =
    let mins = index |> toMins
    in el
        [ DrawCell offset index |> case mouse of
            Nothing -> onMouseDown
            Just _ -> onMouseEnter
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
        <| text <| printMins mins

-- HEADER

header : TTPicker.Model -> Die -> Die -> Timesheet -> Element Msg
header ttPicker vDie today timesheet =
    column headerStyle
    [ row
        [ centerX
        , spacing 10
        , width fill
        , inFront <| Element.map TTPickerMsg <| lazy3 TTPicker.view ttPicker timesheet vDie
        , inFront <| paycheckEstimate timesheet vDie
        ]
        [ prevButton
        , currentDateDisplay vDie
        , nextButton
        ]
    , vDie |> lazy2 dateHeadersWhereTodayIs today
    ]

currentDateDisplay (Die d) = fromRataDie d |> Date.format " MMM y " |> text |> el currentDateStyle

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