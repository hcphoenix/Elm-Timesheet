module TTPicker exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (..)
import TType exposing (..)
import NewTypes exposing (..)
import ViewUtils exposing (..)

import List exposing (range, filter, sum)
import Dict exposing (Dict)
import Array exposing (Array)
import Maybe exposing (withDefault)
import Date exposing (Date)

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

init : Model
init =
    { open = False
    , selection = Work
    }

type alias Model =
    { open : Bool
    , selection : TType
    }

choices = [Work, Sick, Vaca]

type Msg
    = ClickOut
    | Open
    | Select TType

update : Msg -> Model -> Model
update msg model =
    case msg of
        Open ->
            {model | open = True}
        ClickOut ->
            {model | open = False}
        Select tt ->
            {model | open = False, selection = tt}

view : Model -> Dict Int (Array TType) -> Date -> Element Msg
view model timesheet vDate =
    el
        ( border ++ size ++
            [ pointer
            , alignRight
            , Background.color <| colorFromTType model.selection
            ] ++
            if model.open then
                [ below <| viewChoices model.selection
                , onClick ClickOut
                , onLoseFocus ClickOut
                ]
            else
                [ onClick Open
                ]
        )
    <| row [centerY, width fill]
        [ el
            ( font ++
                [centerX]
            )
        <| text <| joinBy '\n'
            [ printTType model.selection
            , hoursOfTTInWeek model.selection (toRataDie vDate) timesheet
                |> (\hrs -> if hrs == 0 then "" else String.fromFloat hrs ++ " hours this week")
            ]
        , el [alignRight] <| text "▼"
        ]

viewChoices : TType -> Element Msg
viewChoices excluding =
    choices |> filter ((/=) excluding) |> List.map
        (\tt -> printTType tt |> text
            |> el
                ( font ++
                    [ width fill
                    , centerY
                    ]
                )
            |> el
                ( size ++
                    [ onClick <| Select tt
                    , pointer
                    , Background.color <| colorFromTTypeWithFade 0.9 tt
                    , mouseOver
                        [ Background.color <| colorFromTTypeWithFade 0.8 tt
                        ]
                    ]
                )
        )
    |> column border

border =
    [ Border.solid
    , Border.width 1
    , Border.color <| gray 0.2
    ]

font =
    [ Font.size 18
    , Font.center
    ]

size =
    [ vw 20
    , vh 5
    ]

hoursOfTTInDie : TType -> Die -> Dict Int (Array TType) -> Float
hoursOfTTInDie tt (Die die) =
    Dict.get die >> Maybe.map (Array.length << Array.filter ((==) tt))
    >> withDefault 0 >> toFloat >> (*) (minuteIncrements/60)

hoursOfTTInWeek : TType -> Die -> Dict Int (Array TType) -> Float
hoursOfTTInWeek tt (Die from) timesheet =
    range from (from+6) |> List.map (\d -> hoursOfTTInDie tt (Die d) timesheet) |> sum