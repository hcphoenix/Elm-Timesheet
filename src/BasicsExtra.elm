module BasicsExtra exposing (..)

import List exposing (..)
import List.Extra exposing (..)
import Dict exposing (Dict)
import Maybe exposing (withDefault)

mapcat = concatMap --branding is important :3

swap (a,b) = (b,a)

eqBy f a b = f a == f b

flip f a b = f b a

curry f (a,b) = f a b

both f a b = f a && f b

updateDict : comparable -> (Maybe v -> v) -> Dict comparable v -> Dict comparable v
updateDict key fn dict = Dict.insert key (fn <| Dict.get key dict) dict

countGroupsBy : (a -> a -> Bool) -> List a -> List (a,Int)
countGroupsBy predicate lst =
    let f (xf,i) l = case l of
            [] -> []
            [x] -> if predicate xf x then [(xf,i+1)] else (xf,i) :: [(x,1)]
            xh::xt -> if predicate xf xh then f (xf,i+1) xt else (xf,i) :: f (xh,1) xt
    in case lst of
        [] -> []
        [x] -> [(x,1)]
        xh::xt -> f (xh,1) xt

map2fill : (a -> b -> c) -> (a -> c) -> List a -> List b -> List c
map2fill zipper filler xs ys =
    case (xs,ys) of
        ([],_) -> []
        ([x],[y]) -> [zipper x y]
        (xh::xt,yh::yt) -> zipper xh yh :: map2fill zipper filler xt yt
        (xf,[]) -> map filler xf

divisibleBy denom = modBy denom >> (==) 0

stride args = if args.from > args.to then [] else args.from :: stride { args|from = args.from + args.by }

liftA2: (a -> b -> c) -> List a -> List b -> List c
liftA2 f x y = x |> mapcat (\ex -> y |> map (f ex))

joinBy = String.fromChar >> String.join