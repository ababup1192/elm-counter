port module Main exposing (Model, Msg(..), init, main, oneToHundred, update, view)

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
    { count : Int, mode : Maybe Mode }


type Mode
    = IncrementMode
    | DecrementMode


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 Nothing, Cmd.none )



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
            ( { model | mode = Just IncrementMode }, Cmd.none )

        Decrement ->
            ( { model | mode = Just DecrementMode }, Cmd.none )

        Stop ->
            ( { model | mode = Nothing }, Cmd.none )

        IncrementN n ->
            ( { model | count = count + n }, Cmd.none )

        DecrementN n ->
            ( { model | count = count - n }, Cmd.none )

        Tick t ->
            case mode of
                Just m ->
                    case m of
                        IncrementMode ->
                            ( { model | count = count + 1 }, Cmd.none )

                        DecrementMode ->
                            ( { model | count = count - 1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
