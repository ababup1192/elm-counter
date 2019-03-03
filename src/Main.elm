port module Main exposing (Model, Msg(..), init, main, oneToHundred, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Increment
    | Decrement
    | IncrementN Int
    | DecrementN Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model, Random.generate IncrementN oneToHundred )

        Decrement ->
            ( model, Random.generate DecrementN oneToHundred )

        IncrementN n ->
            ( model + n, Cmd.none )

        DecrementN n ->
            ( model - n, Cmd.none )


oneToHundred : Random.Generator Int
oneToHundred =
    Random.int 1 100



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ button [ onClick Increment ] [ text "+" ]
        , p [] [ text <| String.fromInt model ]
        , button [ onClick Decrement ] [ text "-" ]
        ]



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
        , subscriptions = \_ -> Sub.none
        }
