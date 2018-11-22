module BasicsExtra exposing (..)

import List exposing (..)

mapcat = concatMap --branding is important :3

swap (a,b) = (b,a)

divisibleBy denom = modBy denom >> (==) 0

liftA2: (a -> b -> c) -> List a -> List b -> List c
liftA2 f x y = x |> mapcat (\ex -> y |> map (f ex))