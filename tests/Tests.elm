module Tests exposing (oneToHundredTest, updateTest, viewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Main exposing (..)
import Random
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)
import Time


countTest : String -> Int -> Msg -> Int -> Test
countTest testCase currentCount msg expectedCount =
    test testCase <|
        \() ->
            update msg (Model currentCount StopMode)
                |> Tuple.first
                |> .count
                |> Expect.equal expectedCount


modeTest : String -> Mode -> Msg -> Mode -> Test
modeTest testCase currentMode msg expectedMode =
    test testCase <|
        \() ->
            update msg (Model 0 currentMode)
                |> Tuple.first
                |> .mode
                |> Expect.equal expectedMode


tickTest : String -> Mode -> Int -> Test
tickTest testCase mode expectedCount =
    test testCase <|
        \() ->
            update (Tick <| Time.millisToPosix 0) (Model 0 mode)
                |> Tuple.first
                |> .count
                |> Expect.equal expectedCount


updateTest : Test
updateTest =
    describe "updateのテスト" <|
        [ describe "n増減"
            [ countTest "カウンタが0のとき、IncrementN 3 されると3になる" 0 (IncrementN 3) 3
            , countTest "カウンタが5のとき、IncrementN 5 されると10になる" 5 (IncrementN 5) 10
            , countTest "カウンタが5のとき、DecrementN 5 されると0になる" 5 (DecrementN 5) 0
            , countTest "カウンタが1のとき、DecrementN -3 されると-2になる" 1 (DecrementN 3) -2
            ]
        , describe "モード切替"
            [ modeTest "+ボタンがクリックされたとき、IncrementModeになる" StopMode Increment IncrementMode
            , modeTest "-ボタンがクリックされたとき、DecrementModeになる" StopMode Decrement DecrementMode
            , modeTest "カウントがクリックされたとき、ストップ" IncrementMode Stop StopMode
            ]
        , describe "定期実行。カウント増減時間が来たかつ、カウントが0のとき"
            [ tickTest "IncrementModeのとき 1になる" IncrementMode 1
            , tickTest "DecrementModeのとき -1になる" DecrementMode -1
            , tickTest "StopModeのとき 0になる" StopMode 0
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
                    view (Model 0 StopMode)
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "0" ]
            , test "カウンタは15を表示している" <|
                \() ->
                    view (Model 15 StopMode)
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "15" ]
            ]
        , describe "増減ボタン"
            [ test "+ボタンはIncrement Msgを発行する" <|
                \() ->
                    view (Model 0 StopMode)
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "+" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Increment
            , test "-ボタンはDecrement Msgを発行する" <|
                \() ->
                    view (Model 0 StopMode)
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "-" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Decrement
            , test "カウントをクリックするとはStop Msgを発行する" <|
                \() ->
                    view (Model 0 IncrementMode)
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Event.simulate Event.click
                        |> Event.expect Stop
            ]
        ]
