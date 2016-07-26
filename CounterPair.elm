module CounterPair exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Counter


-- model


type alias Model =
    { topCounter : Counter.Model
    , bottomCounter : Counter.Model
    }


init : Int -> Int -> Model
init top bottom =
    { topCounter = Counter.init top
    , bottomCounter = Counter.init bottom
    }


initModel : Model
initModel =
    init 0 0



-- update


type Msg
    = Reset
    | Swap
    | Top Counter.Msg
    | Bottom Counter.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initModel

        Swap ->
            { model | topCounter = model.bottomCounter, bottomCounter = model.topCounter }

        Top msg ->
            { model | topCounter = Counter.update msg model.topCounter }

        Bottom msg ->
            { model | bottomCounter = Counter.update msg model.bottomCounter }



-- view


view : Model -> Html Msg
view model =
    div []
        [ App.map Top (Counter.view model.topCounter)
        , App.map Bottom (Counter.view model.bottomCounter)
        , button [ type' "button", onClick Reset ] [ text "Reset" ]
        , button [ type' "button", onClick Swap ] [ text "Swap" ]
        ]


main : Program Never
main =
    App.beginnerProgram { model = initModel, view = view, update = update }
