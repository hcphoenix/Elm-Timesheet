module View exposing (view, startingY)

import BasicsExtra exposing (..)
import Model exposing (..)
import Update exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Events exposing (onMouseDown, onMouseEnter, onMouseLeave)
import Element.Lazy exposing (..)

import Date exposing (Date, Interval(..), Unit(..), toRataDie, fromRataDie)

import List exposing (map, map2, indexedMap, range, repeat, singleton, sum, length, filter)
import Dict exposing (Dict)
import Tuple exposing (pair)

-- COLORS

gray lux = rgb lux lux lux

colorFromTTypeWithAlpha : Float -> TType -> Color
colorFromTTypeWithAlpha alpha tt =
  alpha |> case tt of
    Work -> rgba 0 1 0
    Sick -> rgba 1 0 0
    Vaca -> rgba 0 0 1

colorFromTType : TType -> Color
colorFromTType = colorFromTTypeWithAlpha 0.2

-- TIMES

daysInView : Date -> List Date
daysInView viewingDate = Date.range Day 1 viewingDate (Date.add Days 7 viewingDate)

timesInView : List Int
timesInView = range 0 ((24*60)//minuteIncrements - 1) |> map ((*) minuteIncrements)

mapTimes f = map f timesInView

printMins : Int -> String
printMins time =
  let
    hour = time // 60 |> modBy 12 |> (\h -> if h == 0 then 12 else h) |> String.fromInt
    mins = if time |> divisibleBy 60
                then ""
                else time |> modBy 60 |> String.fromInt |> String.padLeft 2 '0' |> (++) ":"
    ampm = if time < 12*60 then "AM" else "PM"
  in
    hour ++ mins ++ " " ++ ampm

printTType : TType -> String
printTType tt =
    case tt of
        Work -> "Hours Worked"
        Sick -> "Sick Leave"
        Vaca -> "Vacation Time"

-- ELEMENTS

timeLabels = column
    [ width  <| px timeLabelWidth
    , height <| px cellHeight
    ]
    ( mapTimes
        (\mins -> el [] <| text <|
            if mins |> divisibleBy 60 then printMins mins else ""
        )
    )

ttButton : Model -> TType -> Element Msg
ttButton m tt =
  let hoursThisWeek = daysInView m.viewingDate |> map (\date -> m.timesheet |> hoursOfTTInDate tt date) |> sum
  in button (  buttonStyle m.windowWidth
            ++ [ Background.color (colorFromTType tt)
               , if tt == m.selectedTType then Font.bold else Font.regular
               ]
            )
            { onPress = Just (SelectTT tt)
            , label = column [width fill]
                [ el [centerX] <| text <| printTType tt
                , el [centerX] <| text <| if hoursThisWeek == 0 then "" else "(" ++ String.fromFloat hoursThisWeek ++ " hours this week)"
                ]
            }
{-
ttBlock : Model -> Date -> ((Int, Maybe TType), ListExtra.Length) -> List (Element Msg)
ttBlock model date ((mins,mtt),len) =
    let
        attr sub msg i = sub <| msg (toRataDie date, mins + i * minuteIncrements)
        cellStyle i =
            [ width <| colWidths model.windowWidth
            , height <| px <| cellHeight * len
            , pointer
            , i |> if model.dragState == NoDrag then attr onMouseDown DragStartOn else attr onMouseEnter DragOnto
            ]
        emptyCell i =
            el ( [ Border.solid
                 , Border.width 1
                 , Border.color (rgba 0 0 0 0.5)
                 ] ++ cellStyle i
               ) <| text ""

        ttCell tt i = text >> el ([ Background.color (colorFromTType tt) ] ++ cellStyle i)
    in case mtt of
        Nothing -> range 0 len |> map emptyCell
        Just tt -> repeat (len-2) "" |>
            (++)(  printTType tt
                :: if len == 1 then [] else [ printMins mins ++ " - " ++ printMins (mins + len*minuteIncrements) ]
                ) |> indexedMap (ttCell tt)
-}
timeCell : Model -> DieMins -> Element Msg
timeCell model (die,mins) =
    let
        isHour = mins |> divisibleBy 60
        isHover = model.hoveringCell == Just (die,mins)
    in el
    ( [ onMouseDown <| DragStartOn (die,mins)
        , onMouseEnter <| MouseEnterCell (die,mins)
        , width <| colWidths model.windowWidth
        , height <| px cellHeight
        , pointer
        ]
    ++ case Dict.get (die,mins) model.timesheet of
        Just tt ->  [ Background.color (colorFromTType tt)
                    ]
        Nothing ->  [ Border.solid
                    , Border.widthEach
                        { left   = 0
                        , right  = 0
                        , bottom = 0
                        , top    = if isHour then 2 else 1
                        }
                    , Border.color <| gray <| if isHour then 0.4 else 0.8
                    , Font.color <| gray 0.5
                    , Background.color <| if isHover then colorFromTTypeWithAlpha 0.1 model.selectedTType else gray 0.98
                    ]
      )
      <| text <|
        let
            getTT m = Dict.get (die,m) model.timesheet
            nextTT = getTT << (+) minuteIncrements
        in case getTT mins of
            Just tt ->
                --First cell in block
                if Just tt /= (getTT <| mins - minuteIncrements) then
                    printTType tt
                --Second cell in block
                else if Just tt /= (getTT <| mins - 2*minuteIncrements) then
                    let endOfBlock n = if getTT n == nextTT n then endOfBlock <| n + minuteIncrements else n
                    in  printMins (mins - minuteIncrements) ++ " - " ++ printMins ((endOfBlock mins) + minuteIncrements)
                --All other cells in block
                else ""
            Nothing ->
                if isHover && model.dragState /= Erasing then
                    printMins mins ++
                        case die - toRataDie model.todaysDate of
                            1  -> " Tomorrow"
                            0  -> " Today"
                        -- -1 -> " Yesterday" --Elm parser bug???
                            _  -> Date.format " EEE" (fromRataDie die)
                else ""


view : Model -> Html Msg
view model =
  let

    vDate = model.viewingDate
    mapDays f = map f <| daysInView vDate

    columns = (::) timeLabels <| mapDays <| toRataDie >>
        (\die -> column columnStyle <| mapTimes <| timeCell model << pair die)

    header =
        let
            currentDateDisplay = vDate |> el currentDateStyle << text << Date.format " MMM y "

            prevButton = button (buttonStyle model.windowWidth) {onPress = Just <| PrevWeek, label = text "← Prev Week"}
            nextButton = button (buttonStyle model.windowWidth) {onPress = Just <| NextWeek, label = text "Next Week →"}

            dateHeaders = row []
                <| el
                    [width <| px timeLabelWidth]
                    (text "")
                :: (mapDays <| Date.format "EEE d" >> text >> el (thStyle model.windowWidth))

        in column headerStyle
            [ row
                [ centerX
                , spacing 10
                ]
                [ prevButton
                , currentDateDisplay
                , nextButton
                ]
            , row [centerX] (allTTypes |> map (ttButton model))
            , dateHeaders
            ]
  in layout globalStyle <| row [inFront header] columns

-- SIZES

startingY = 9 * (60 / minuteIncrements) * cellHeight -- - headerHeight

headerHeight = 128

cellHeight = 20

timeLabelWidth = 100

colWidths = toFloat >> (*) 0.18 >> round >> px

buttonWidth ww = ww // 5 |> px
buttonHeight = 40 |> px

-- STYLES

globalStyle =
    [ Font.center
    , Font.family [ Font.sansSerif ]
    ]

headerStyle =
    headerBorder ++
    [ Font.size 18
    , Background.color <| rgb 1 1 1
    , height <| px headerHeight
    , spacing 10
    , padding 10
    , htmlAttribute <| Attr.style "position" "sticky"
    , htmlAttribute <| Attr.style "top" "0"
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

buttonStyle ww =
    [ height buttonHeight
    , width <| buttonWidth ww
    , Border.solid
    , Border.rounded 6
    , Border.width 1
    , Border.color <| rgba 0 0 0 0.2
    ]

thStyle ww =
    headerBorder ++
    [ Background.color <| rgb 1 1 1
    , width <| colWidths ww
    ]

columnStyle =
    [ moveDown <| toFloat headerHeight
    , Border.solid
    , Border.width 1
    , Border.color <| gray 0.6
    ]