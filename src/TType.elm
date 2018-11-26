module TType exposing (..)
import Element exposing (Color, rgb, rgba)

type TType
  = Work
  | Sick
  | Vaca
  | None

printTType : TType -> String
printTType tt =
    case tt of
        Work -> "Hours Worked"
        Sick -> "Sick Leave"
        Vaca -> "Vacation Time"
        None -> ""

colorFromTTypeWithFade : Float -> TType -> Color
colorFromTTypeWithFade a tt =
  case tt of
    Work -> rgb a 1 a
    Sick -> rgb 1 a a
    Vaca -> rgb a a 1
    None -> rgba 0 0 0 0

colorFromTType : TType -> Color
colorFromTType = colorFromTTypeWithFade 0.8