{- Excerpts from circuithub/elm-list-extra, which has yet to be updated for 0.19 -}
module ListExtra exposing (zipMap, takeWhile, dropWhile, span, group, groupBy, Length)

import List exposing (..)
import Tuple

{-| Take two lists and returns a list of corresponding pairs
-}
zip : List a -> List b -> List (a,b)
zip = map2 Tuple.pair

zipMap f list = map f list |> zip list

{-| Take elements in order as long as the predicate evaluates to `True` -}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []

{-| Drop elements in order as long as the predicate evaluates to `True` -}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs else list

{-| Take a predicate and a list, return a tuple. The first part of the tuple is longest prefix of that list, for each element of which the predicate holds. The second part of the tuple is the remainder of the list. `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`.
    span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
    span (< 5) [1,2,3] == ([1,2,3],[])
    span (< 0) [1,2,3] == ([],[1,2,3])
-}
span : (a -> Bool) -> List a -> (List a, List a)
span p xs = (takeWhile p xs, dropWhile p xs)

group = groupBy (==)

type alias Length = Int

groupBy : (a -> a -> Bool) -> List a -> List (a, Length)
groupBy eq xsΔ =
  case xsΔ of
    [] -> []
    (x::xs) -> let
                  (ys,zs) = span (eq x) xs
                  len = 1 + List.length ys
               in (x,len)::groupBy eq zs