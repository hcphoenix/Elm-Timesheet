module TType exposing (..)
import Element exposing (Color, rgb, rgba)

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