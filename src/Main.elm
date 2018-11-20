import Browser
import Browser.Dom
import Browser.Events

import Html exposing (Html)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Events exposing (onMouseDown, onMouseEnter)
import Element.Lazy exposing (..)

import Date exposing (Date, Interval(..), Unit(..), toRataDie)
import Time exposing (Month(..))

import List exposing (map, map2, indexedMap, range, repeat, singleton, sum, length, filter)
import ListExtra exposing (..)
import Tuple exposing (..)
import Dict exposing (Dict)
import Maybe exposing (Maybe(..), withDefault, andThen)
import Task
import Json.Decode exposing (succeed)

import Debug

main : Program () Model Msg
main = Browser.element
      { init = always init
      , view = view
      , update = update
      , subscriptions = always subs
      }

-- aliases and helper functions

mapcat = List.concatMap

type alias Mins = Int

swap (a,b) = (b,a)

-- MODEL

type alias Model =
  { allDates : Dict Int (Dict Mins TType)
  , viewingDate : Date
  , selectedTType : TType
  , dragState   : DragState
  , windowWidth : Int
  , windowHeight : Int
  }

type TType
  = Work
  | Sick
  | Vaca
allTTypes = [Work, Sick, Vaca]

type DragState
  = NoDrag
  | Writing
  | Erasing

init : ( Model, Cmd Msg )
init =
    (   { allDates = Dict.empty
        , viewingDate = Date.fromCalendarDate 2018 Jan 1
        , dragState   = NoDrag
        , selectedTType = Work
        , windowWidth = 640
        , windowHeight = 480
        }
    , Cmd.batch
        [ Task.perform ChangeDateTo Date.today
        , Task.perform (\vp -> WindowResize (round vp.scene.width) (round vp.scene.height)) Browser.Dom.getViewport
        ]
    )

minuteIncrements = 15

getTTsForDate : Model -> Date -> Dict Mins TType
getTTsForDate m d =
  Dict.get (d |> toRataDie) m.allDates |> withDefault Dict.empty

getTT : Model -> Date -> Mins -> Maybe TType
getTT model date mins =
  getTTsForDate model date |> Dict.get mins

hoursOfTTInDate : TType -> Dict Mins TType -> Float
hoursOfTTInDate tt b =
  b |> Dict.values |> filter ((==) tt) |> length |> toFloat |> (*) (minuteIncrements/60)

-- SUBS
subs = Sub.batch
    [ Browser.Events.onResize  <| WindowResize
    , Browser.Events.onMouseUp <| succeed DragEnd
    ]

-- UPDATE

type Msg
  = ChangeDateTo Date
  | PrevWeek
  | NextWeek
  | DragStartOn Date Mins
  | DragOnto Date Mins
  | DragEnd
  | SetTT TType
  | WindowResize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    changeDateBy quantity unit = {model | viewingDate = model.viewingDate |> Date.add unit quantity}
    write date f = model.allDates |> Dict.insert (toRataDie date) (getTTsForDate model date |> f)
  in swap <| pair Cmd.none <| Debug.log "New model:" <| case msg of

        PrevWeek -> changeDateBy -7 Days

        NextWeek -> changeDateBy 7 Days

        ChangeDateTo date ->
            { model
            | viewingDate = Date.floor Date.Monday date
            }

        SetTT t ->
            { model
            | selectedTType = t
            }

        DragStartOn date mins ->
            let tt = getTT model date mins
            in  if tt == Just model.selectedTType
                then    { model
                        | allDates = write date <| Dict.remove mins
                        , dragState = Erasing
                        }
                else    { model
                        | allDates = write date <| Dict.insert mins model.selectedTType
                        , dragState = Writing
                        }

        DragOnto date mins ->
            { model
            | allDates = write date  <| if model.dragState == Erasing
                                        then Dict.remove mins
                                        else Dict.insert mins model.selectedTType
            }

        DragEnd ->
            { model
            | dragState = NoDrag
            }

        WindowResize w h ->
            { model
            | windowWidth  = w
            , windowHeight = h
            }

-- VIEW

daysInView : Model -> List Date
daysInView model = Date.range Day 1 model.viewingDate (Date.add Days 7 model.viewingDate)

timesInView : List Mins
timesInView = range 0 ((24*60)//minuteIncrements - 1) |> map ((*) minuteIncrements)

printTime : Mins -> String
printTime time =
  let
    hour = String.fromInt (time // 60 |> modBy 12 |> (\h -> if h == 0 then 12 else h))
    mins = String.fromInt (time |> modBy 60) |> String.padLeft 2 '0'
    ampm = if time < 12*60 then "AM" else "PM"
  in
    hour ++ ":" ++ mins ++ " " ++ ampm

printTType : TType -> String
printTType tt =
  case tt of
    Work -> "Hours Worked"
    Sick -> "Sick Leave"
    Vaca -> "Vacation Time"

colorFromTType : TType -> Color
colorFromTType tt =
  0.2 |> case tt of
    Work -> rgba 0 1 0
    Sick -> rgba 1 0 0
    Vaca -> rgba 0 0 1

ttButton : Model -> TType -> Element Msg
ttButton m tt =
  let hoursThisWeek = daysInView m |> map (\d -> getTTsForDate m d |> hoursOfTTInDate tt) |> sum
  in button (  buttonStyle m.windowWidth
            ++ [ Background.color (colorFromTType tt)
               , if tt == m.selectedTType then Font.bold else Font.regular
               ]
            )
            { onPress = Just (SetTT tt)
            , label = column [width fill]
                            [ el [centerX] <| text <| printTType tt
                            , el [centerX] <| text <| if hoursThisWeek == 0 then "" else "(" ++ String.fromFloat hoursThisWeek ++ " hours this week)"
                            ]
            }

ttBlock : Model -> Date -> ((Mins, Maybe TType), ListExtra.Length) -> List (Element Msg)
ttBlock model date ((mins,mtt),len) =
    let
        attr sub msg i = sub <| msg date <| mins + i * minuteIncrements
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
                :: if len == 1 then [] else [ printTime mins ++ " - " ++ printTime (mins + len*minuteIncrements) ]
                ) |> indexedMap (ttCell tt)

timeCell : Model -> Date -> Mins -> Element Msg
timeCell model date mins =
  el  (  [ if model.dragState == NoDrag
                then onMouseDown <| DragStartOn date mins
                else onMouseEnter <| DragOnto date mins
         , width (model.windowWidth |> colWidths)
         , height <| px cellHeight
         , pointer
         ]
      ++ case getTT model date mins of
            Just tt -> [ Background.color (colorFromTType tt)
                       ]
            Nothing -> [ Border.solid
                       , Border.width 1
                       , Border.color <| rgba 0 0 0 0.5
                       ]
      )
      <| text <| case getTT model date mins of
            Just tt ->
                if Just tt /= getTT model date (mins - minuteIncrements) then
                    printTType tt
                else if Just tt /= getTT model date (mins - 2*minuteIncrements) then
                    let endOfBlock n = if getTT model date n == getTT model date (n+minuteIncrements) then endOfBlock <| n+minuteIncrements else n
                    in  printTime (mins - minuteIncrements) ++ " - " ++ printTime ((endOfBlock mins) + minuteIncrements)
                else ""
            Nothing -> ""

timeLabelWidth = 100
cellHeight = 20

timeLabels = column [ width  <| px timeLabelWidth
                    , height <| px cellHeight
                    ]
                    ( timesInView |> map
                        (\mins -> el [] <| text <|
                            if mins |> modBy 60 |> (==) 0 then printTime mins else ""
                        )
                    )
view : Model -> Html Msg
view model =
  let
    columns = timeLabels :: (daysInView model |> map (\day -> column [moveDown <| toFloat headerHeight] (timesInView
        |> map (timeCell model day))))
      --|> zipMap (getTT model day) |> groupBy (\x y -> second x == second y) |> mapcat (ttBlock model day))))
    header =
        let
            currentDateDisplay = model.viewingDate |> lazy (el currentDateStyle << text << Date.format " MMM y ")

            prevButton = button (buttonStyle model.windowWidth) {onPress = Just <| PrevWeek, label = text "< Prev Week"}
            nextButton = button (buttonStyle model.windowWidth) {onPress = Just <| NextWeek, label = text "Next Week >"}

            dateHeaders = row []
                ( el
                    [width <| px timeLabelWidth]
                    (text "")
                :: (model |> daysInView |> map (Date.format "EEE d" >> text >> el (thStyle model.windowWidth)))
                )
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

-- STYLES

globalStyle =
    [ Font.center
    , Font.family [ Font.sansSerif ]
    ]

headerHeight = 130

headerStyle =
    [ Font.size 18
    , Background.color <| rgb 1 1 1
    , height <| px headerHeight
    , spacing 10
    , padding 10
    ]

currentDateStyle =
    [ Font.size 32
    ]

buttonStyle ww =
    [ height <| px 40
    , width <| px <| ww//5
    , Border.solid
    , Border.rounded 6
    , Border.width 1
    , Border.color <| rgba 0 0 0 0.2
    ]

thStyle ww =
    [ Background.color <| rgb 1 1 1
    , width <| colWidths ww
    ]

colWidths = toFloat >> (*) 0.18 >> round >> px