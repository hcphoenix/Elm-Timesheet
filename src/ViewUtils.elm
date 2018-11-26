module ViewUtils exposing (..)
import Element exposing (Color, rgb, rgba, htmlAttribute)
import Html.Attributes exposing (style)

invisible = rgba 0 0 0 0
gray lux = rgb lux lux lux
white = gray 1

vw i = cssStyle "width" <| String.fromInt i ++ "vw"
vh i = cssStyle "height" <| String.fromInt i ++ "vh"

cssStyle name val = htmlAttribute <| style name val --this ought to be pointfree-able but i'm dumb