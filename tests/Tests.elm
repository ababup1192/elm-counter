module Tests exposing (oneToHundredTest, updateTest, viewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Main exposing (..)
import Random
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)


updateTest : Test
updateTest =
    describe "updateのテスト" <|
        [ describe "n増えるカウンタ"
            [ test "カウンタが0のとIncrementN 3されると3になる" <|
                \() ->
                    update (IncrementN 3) (Model 0 Nothing)
                        |> Tuple.first
                        |> .count
                        |> Expect.equal 3
            , test "カウンタが5のとIncrementN 5されると10になる" <|
                \() ->
                    update (IncrementN 5) (Model 5 Nothing)
                        |> Tuple.first
                        |> .count
                        |> Expect.equal 10
            ]
        , describe "n減るカウンタ"
            [ test "カウンタが5のとDecrementN 5されると0になる" <|
                \() ->
                    update (DecrementN 5) (Model 5 Nothing)
                        |> Tuple.first
                        |> .count
                        |> Expect.equal 0
            , test "カウンタが1のとDecrementN 3されると-2になる" <|
                \() ->
                    update (DecrementN 3) (Model 1 Nothing)
                        |> Tuple.first
                        |> .count
                        |> Expect.equal -2
            ]
        ]


oneToHundredTest : Test
oneToHundredTest =
    describe "oneToHundredのテスト"
        [ fuzz (intRange -100000 100000) "どんなシード値でも、1-100までの数値を出す" <|
            \randomlyGeneratedNum ->
                Random.step oneToHundred (Random.initialSeed randomlyGeneratedNum)
                    |> Tuple.first
                    |> Expect.all
                        [ Expect.atLeast 1
                        , Expect.atMost 100
                        ]
        ]


viewTest : Test
viewTest =
    describe "viewのテスト" <|
        [ describe "カウンタの表示"
            [ test "カウンタは0を表示している" <|
                \() ->
                    view (Model 0 Nothing)
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "0" ]
            , test "カウンタは15を表示している" <|
                \() ->
                    view (Model 15 Nothing)
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "15" ]
            ]
        , describe "増減ボタン"
            [ test "+ボタンはIncrement Msgを発行する" <|
                \() ->
                    view (Model 0 Nothing)
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "+" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Increment
            , test "-ボタンはDecrement Msgを発行する" <|
                \() ->
                    view (Model 0 Nothing)
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "-" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Decrement
            ]
        ]
