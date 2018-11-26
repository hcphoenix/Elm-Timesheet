module NewTypes exposing (..)
import BasicsExtra exposing (..)
import Date
import List exposing (range, map)

type Die = Die Int

type Index = Index Int

type Mins = Mins Int

minuteIncrements = 15

iRange lo hi = range lo hi |> map Index

minIndex (Index a) (Index b) = Index <| min a b
maxIndex (Index a) (Index b) = Index <| max a b

toRataDie = Date.toRataDie >> Die
next (Index i) = Index (i+1)
prev (Index i) = Index (i-1)
intFromDie (Die d) = d

toMins (Index i) = i * minuteIncrements |> Mins
isHour (Mins m) = m |> divisibleBy 60