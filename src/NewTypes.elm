module NewTypes exposing (..)
import BasicsExtra exposing (..)
import Date exposing (Date, Unit(..), Interval(..))
import Dict.Any exposing (AnyDict, get)
import List exposing (range, map, filter, sum)
import Tuple exposing (first, second)
import Maybe exposing (withDefault)
import Element exposing (Color, rgb, rgba)

type Index = Index Int

type alias EntriesForDate = AnyDict Int Index (Index, TType)

type alias Timesheet = AnyDict Int Date EntriesForDate

minuteIncrements = 15

tsGet date = Dict.Any.get date >> withDefault (Dict.Any.empty (\(Index i) -> i))

dateToWeek date = Date.range Day 1 date (Date.add Days 7 date)

hoursOfTTInDate : TType -> EntriesForDate -> Float
hoursOfTTInDate tt =
    Dict.Any.toList
    >> filter (second >> second >> (==) tt)
    >> List.map (\((Index start),((Index end),_)) -> end - start |> (*) minuteIncrements |> toFloat |> (*) (1/60))
    >> sum

hoursOfTTInWeek : TType -> Timesheet -> Date -> Float
hoursOfTTInWeek tt timesheet =
    dateToWeek
    >> List.map (\date -> get date timesheet |> Maybe.map (hoursOfTTInDate tt) |> withDefault 0)
    >> sum

iRange lo hi = range lo hi |> map Index

minIndex (Index a) (Index b) = Index <| min a b
maxIndex (Index a) (Index b) = Index <| max a b

next (Index i) = Index (i+1)
prev (Index i) = Index (i-1)

type TType
    = Work
    | Sick
    | Vaca

printTType : TType -> String
printTType tt =
    case tt of
        Work -> "Hours Worked"
        Sick -> "Sick Leave"
        Vaca -> "Vacation Time"

colorFromTTypeWithFade : Float -> TType -> Color
colorFromTTypeWithFade a tt =
    case tt of
        Work -> rgb a 1 a
        Sick -> rgb 1 a a
        Vaca -> rgb a a 1

colorFromTType : TType -> Color
colorFromTType = colorFromTTypeWithFade 0.8