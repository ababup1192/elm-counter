port module Main exposing (Mode(..), Model, Msg(..), init, main, oneToHundred, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Time



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { count : Int, mode : Mode }


type Mode
    = IncrementMode
    | DecrementMode
    | StopMode


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 StopMode, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Increment
    | Decrement
    | Stop
    | IncrementN Int
    | DecrementN Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ count, mode } as model) =
    case msg of
        Increment ->
            ( { model | mode = IncrementMode }, Cmd.none )

        Decrement ->
            ( { model | mode = DecrementMode }, Cmd.none )

        Stop ->
            ( { model | mode = StopMode }, Cmd.none )

        IncrementN n ->
            ( { model | count = count + n }, Cmd.none )

        DecrementN n ->
            ( { model | count = count - n }, Cmd.none )

        Tick _ ->
            ( { model | count = updateCountByMode mode count }, Cmd.none )


updateCountByMode : Mode -> Int -> Int
updateCountByMode mode currentCount =
    case mode of
        IncrementMode ->
            currentCount + 1

        DecrementMode ->
            currentCount - 1

        StopMode ->
            currentCount


oneToHundred : Random.Generator Int
oneToHundred =
    Random.int 1 100



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view { count } =
    div [ class "container" ]
        [ button [ onClick Increment ] [ text "+" ]
        , p [ style "cursor" "pointer", onClick Stop ] [ text <| String.fromInt count ]
        , button [ onClick Decrement ] [ text "-" ]
        ]



-- ---------------------------
-- SUBSCRIPTIONS
-- ---------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "カウンター"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }
