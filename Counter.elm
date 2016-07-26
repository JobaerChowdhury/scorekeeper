module Counter exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


-- model


type alias Model =
    Int


init : Int -> Model
init i =
    i


initModel : Model
initModel =
    init 1



-- update


type Msg
    = Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1

        Dec ->
            model - 1



--view


view : Model -> Html Msg
view model =
    div []
        [ button [ type' "button", onClick Dec ] [ text "-" ]
        , div [ countStyle ] [ text (toString model) ]
        , button [ type' "button", onClick Inc ] [ text "+" ]
        ]


countStyle : Attribute msg
countStyle =
    style
        [ ( "font-size", "20px" )
        , ( "font-family", "monospace" )
        , ( "display", "inline-block" )
        , ( "width", "50px" )
        , ( "text-align", "center" )
        ]


main : Program Never
main =
    App.beginnerProgram { model = initModel, view = view, update = update }
