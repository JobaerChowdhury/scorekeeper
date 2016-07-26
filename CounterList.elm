module CounterPair exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Counter


-- model


type alias Model =
    { counters : List IndexedCounter
    , uid : Int
    }


type alias IndexedCounter =
    { id : Int
    , model : Counter.Model
    }


initModel : Model
initModel =
    { counters = []
    , uid = 0
    }



-- update


type Msg
    = Reset
    | Add
    | Remove Int
    | Send Int Counter.Msg


updateHelp : Int -> Counter.Msg -> IndexedCounter -> IndexedCounter
updateHelp targetId msg { id, model } =
    IndexedCounter id
        (if targetId == id then
            Counter.update msg model
         else
            model
        )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initModel

        Add ->
            let
                newId =
                    model.uid + 1

                newCounter =
                    IndexedCounter model.uid (Counter.init 0)
            in
                { model | counters = newCounter :: model.counters, uid = newId }

        Remove id ->
            { model | counters = List.filter (\ic -> ic.id /= id) model.counters }

        Send id msg ->
            { model | counters = List.map (updateHelp id msg) model.counters }



-- view


view : Model -> Html Msg
view model =
    div []
        [ showCounters model
        , button [ type' "button", onClick Reset ] [ text "Reset all" ]
        , button [ type' "button", onClick Add ] [ text "Add counter" ]
        ]


showCounters : Model -> Html Msg
showCounters model =
    ul [] <| List.map showCounter model.counters


showCounter : IndexedCounter -> Html Msg
showCounter counter =
    span []
        [ App.map (Send counter.id) (Counter.view counter.model)
        , button [ type' "button", onClick (Remove counter.id) ] [ text "Remove" ]
        ]


main : Program Never
main =
    App.beginnerProgram { model = initModel, view = view, update = update }
