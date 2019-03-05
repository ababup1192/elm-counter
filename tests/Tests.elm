module Tests exposing (oneToHundredTest, updateTest, viewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Main exposing (..)
import Random
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)


countTest : String -> Int -> Msg -> Int -> Test
countTest testCase initialCount msg expectedCount =
    test testCase <|
        \() ->
            update msg (Model initialCount Nothing)
                |> Tuple.first
                |> .count
                |> Expect.equal expectedCount


updateTest : Test
updateTest =
    describe "updateのテスト" <|
        [ countTest "カウンタが0のとIncrementN 3されると3になる" 0 (IncrementN 3) 3
        , countTest "カウンタが5のとIncrementN 5されると10になる" 5 (IncrementN 5) 10
        , countTest "カウンタが5のとDecrementN 5されると0になる" 5 (DecrementN 5) 0
        , countTest "カウンタが1のとDecrementN -3されると-2になる" 1 (DecrementN 3) -2
        , test "+ボタンがクリックされたとき、IncrementModeになる" <|
            \() ->
                update Increment (Model 0 Nothing)
                    |> Tuple.first
                    |> .mode
                    |> Expect.equal (Just IncrementMode)
        , test "-ボタンがクリックされたとき、DecrementModeになる" <|
            \() ->
                update Decrement (Model 0 Nothing)
                    |> Tuple.first
                    |> .mode
                    |> Expect.equal (Just DecrementMode)
        , test "カウントがクリックされたとき、ストップする" <|
            \() ->
                update Stop (Model 0 (Just IncrementMode))
                    |> Tuple.first
                    |> .mode
                    |> Expect.equal Nothing
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
            , test "カウントをクリックするとはStop Msgを発行する" <|
                \() ->
                    view (Model 0 (Just IncrementMode))
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Event.simulate Event.click
                        |> Event.expect Stop
            ]
        ]
