module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


-- model


type alias Model =
    Int


initModel : Model
initModel =
    0



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
        [ button [ type' "button", onClick Inc ] [ text "Inc" ]
        , text (toString model)
        , button [ type' "button", onClick Dec ] [ text "Dec" ]
        ]


main : Program Never
main =
    App.beginnerProgram { model = initModel, view = view, update = update }
