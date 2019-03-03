port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



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
            update (IncrementN 1) model

        Decrement ->
            update (DecrementN 1) model

        IncrementN n ->
            ( model + n, Cmd.none )

        DecrementN n ->
            ( model - n, Cmd.none )



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
