module View exposing (view, startingY)

import BasicsExtra exposing (..)
import Model exposing (..)
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
import Element.Events exposing (onMouseDown, onMouseUp, onMouseEnter, onMouseLeave)
import Element.Lazy exposing (..)
import Element.Keyed as Keyed

import Date exposing (Date, Interval(..), Unit(..), fromRataDie)

import Dict.Any exposing (toList, get, empty)
import List exposing (map, map2, indexedMap, range, repeat, singleton, sum, length, filter)
import Tuple exposing (first, second, pair, mapFirst, mapSecond)
import Maybe exposing (withDefault)

view : Model -> Html Msg
view model =
    layout globalStyle <| row
        [ inFront <| header model
        , behindContent timeLabels
        , cssStyle "-moz-user-select" "none"
        ] <| indexedMap (timeColumn model) <| dateToWeek model.viewingDate

globalStyle =
    [ Font.center
    , Font.family [ Font.sansSerif ]
    , Font.size 12
    ]

-- TIMES

printMins : Index -> String
printMins (Index i) =
    let
        time = i * minuteIncrements
        hour = time // 60 |> modBy 12 |> (\h -> if h == 0 then 12 else h) |> String.fromInt
        mins = time |> modBy 60 |> (\m -> if m == 0 then "" else String.fromInt m |> String.padLeft 2 '0' |> (++) ":")
        ampm = if time < 12*60 || time == 60*24 then "AM" else "PM"
    in hour ++ mins ++ " " ++ ampm

printDateWhereTodayIs : Date -> Date -> String
printDateWhereTodayIs today date =
    case Date.diff Days date today of
        1  -> "Tomorrow"
        0  -> "Today"
    -- -1  -> "Yesterday" --Elm parser bug???
        _  -> Date.format "EEE d" date

-- VARIOUS

paycheckEstimate : Timesheet -> Date -> Element Msg
paycheckEstimate ts vd =
    let total = dateToWeek vd |> mapcat
            (\d -> tsGet d ts |> Dict.Any.toList |> map (\(_,((Index i),_)) -> i))
            |> sum |> toFloat |> (*) (12.5 * minuteIncrements/60) |> round
    in el [] <| if total == 0 then Element.none else 
        text <| (++) "Estimated pay for this week: $" <| String.fromInt total

timeLabels =
    column
        [ width fill
        , moveDown headerHeight
        , spacing cellHeight
        , padding cellHeight
        ]
        ( range 0 23 |> map
            ( (*) (60//minuteIncrements) >> Index >> printMins >> text >> el
                [ width fill
                , height <| px <| cellHeight * (45//minuteIncrements)
                , Font.size 40
                , Font.center
                , Font.color <| gray 0.8
                ]
            )
        )

-- TIMESHEET

timeColumn : Model -> Int -> Date -> Element Msg
timeColumn model offset d =
    el  (   [ moveDown <| toFloat headerHeight
            , onMouseEnter <| EnterCol offset
            ]
        ++ (map inFront <| map (\(s,(e,tt)) -> ttBlock offset s e tt False) <| toList <| tsGet d model.timesheet)
        ++ case model.drawing of
            Nothing -> []
            Just (start,end) ->
                if offset==model.hoverCol then
                    [ inFront <| ttBlock offset (minIndex start end) (maxIndex start end) model.ttPicker.selection True ]
                else []
        ) <| lazy3 cells model.drawing model.ttPicker.selection offset

ttBlock offset (Index start) (Index end) tt isDragBlock =
    joinBy '\n'
        [ printTType tt
        , printMins (Index start) ++ " - " ++ printMins (end+1 |> Index)
        ]
    |> text |> el
        (   [ colWidths
            , height <| px <| cellHeight * (end - start + 1)
            , moveDown <| toFloat <| cellHeight * start
            , clipX
            , clipY
            , Border.solid
            , Border.width 1
            , Border.color <| gray 0.3
            , Background.color <| colorFromTType tt
            , Font.size 16
            ]
        ++  if isDragBlock then
                [ cssStyle "pointer-events" "none"
                ] 
            else 
                [ inFront <| el
                    [ alignRight
                    , Font.bold
                    , onMouseDown <| XBlock (Index start) offset
                    , pointer
                    ] <| text "X"
                ]
        )
        

cells : Maybe (Index,Index) -> TType -> Int -> Element Msg
cells mouse sTT offset =
    iRange 0 numTimesInDay |> map (cell mouse sTT offset) |> column
        [ Border.solid
        , Border.width 1
        , Border.color <| gray 0.6
        ]

cell : Maybe (Index,Index) -> TType -> Int -> Index -> Element Msg
cell mouse sTT offset (Index i) =
    let mins = i * minuteIncrements
    in printMins (Index i) |> text |> el
        [ DrawCell (Index i) |> case mouse of
            Nothing -> onMouseDown
            Just _  -> onMouseEnter
        , onMouseUp DrawEnd
        , colWidths
        , height <| px cellHeight
        , pointer
        , Border.solid
        , Border.widthEach
            { left   = 0
            , right  = 0
            , bottom = 0
            , top    = if mins |> divisibleBy 60 then 2 else 1
            }
        , Border.color <| gray <| if mins |> divisibleBy 60 then 0.4 else 0.8
        , Font.color invisible
        , Background.color <| invisible
        , mouseOver
            [ Background.color <| colorFromTTypeWithFade 0.9 sTT
            , Font.color <| gray 0.7
            ]
        ]

-- HEADER

header : Model -> Element Msg
header model =
    column headerStyle
    [ row
        [ centerX
        , spacing 10
        , width fill
        , inFront <| Element.map TTPickerMsg <| lazy3 TTPicker.view model.ttPicker model.timesheet model.viewingDate
        , inFront <| paycheckEstimate model.timesheet model.viewingDate
        ]
        [ prevButton
        , currentDateDisplay model.viewingDate
        , nextButton
        ]
    , lazy dateHeaders {vDate = model.viewingDate, today = model.today, hoverCol = model.hoverCol, shifts = model.recentShifts}
    ]

currentDateDisplay = Date.format " MMM y " >> text >> el
    [ Font.size 32
    ]

prevButton = button buttonStyle {onPress = Just <| PrevWeek, label = text "← Prev Week"}
nextButton = button buttonStyle {onPress = Just <| NextWeek, label = text "Next Week →"}

dateHeaders {vDate, today, hoverCol, shifts} =
    dateToWeek vDate |> indexedMap
        ( \colNum date -> date |> printDateWhereTodayIs today |> text
            |> el
                ( thStyle
                ++ if date /= today then [] else [Font.bold]
                {-
                ++ if colNum /= hoverCol then [] else
                    [ below <| recentShiftPicker hoverCol shifts
                    ]
                -}
                )
        )
    |> row [centerX, width fill]

recentShiftPicker offset =
    map (\(iStart,iEnd) -> el
            [ width fill
            , Background.color white
            ] <| text <| "+ " ++ printMins iStart ++ " - " ++ printMins iEnd
        )
    >> column
        [ colWidths
        , cssStyle "position" "fixed"
        , cssStyle "left" <| String.fromInt <| offset*14
        , cssStyle "top" <| String.fromInt <| headerHeight
        ]

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