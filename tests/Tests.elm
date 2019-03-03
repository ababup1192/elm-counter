module Tests exposing (updateTest, viewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (containing, tag, text)


continuousIncrementDecrement : Msg -> Int -> Model -> Model
continuousIncrementDecrement msg num currentCounter =
    let
        nextCounter =
            update msg currentCounter |> Tuple.first
    in
    if num == 0 then
        currentCounter

    else
        continuousIncrementDecrement msg (num - 1) nextCounter


updateTest : Test
updateTest =
    describe "updateのテスト" <|
        [ fuzz (intRange 0 100) "同じ数だけIncrementをしてDecrementをすると元の数字に戻る" <|
            \randomlyGeneratedNum ->
                continuousIncrementDecrement Increment randomlyGeneratedNum 0
                    |> continuousIncrementDecrement Decrement randomlyGeneratedNum
                    |> Expect.equal 0
        , describe "増えるカウンタ"
            [ test "カウンタが0のときIncrementされると1になる" <|
                \() ->
                    update Increment 0
                        |> Tuple.first
                        |> Expect.equal 1
            , test "カウンタが5のときIncrementされると6になる" <|
                \() ->
                    update Increment 5
                        |> Tuple.first
                        |> Expect.equal 6
            , test "カウンタが0のとき、5回Incrementされると5になる" <|
                \() ->
                    continuousIncrementDecrement Increment 5 0
                        |> Expect.equal 5
            ]
        , describe "減るカウンタ"
            [ test "カウンタが0のとDecrementされると-1になる" <|
                \() ->
                    update Decrement 0
                        |> Tuple.first
                        |> Expect.equal -1
            , test "カウンタが5のとDecrementされると4になる" <|
                \() ->
                    update Decrement 5
                        |> Tuple.first
                        |> Expect.equal 4
            , test "カウンタが5のとき、5回Decrementされると5になる" <|
                \() ->
                    continuousIncrementDecrement Decrement 5 5
                        |> Expect.equal 0
            ]
        , describe "n増えるカウンタ"
            [ test "カウンタが0のとIncrementN 3されると3になる" <|
                \() ->
                    update (IncrementN 3) 0
                        |> Tuple.first
                        |> Expect.equal 3
            , test "カウンタが5のとIncrementN 5されると10になる" <|
                \() ->
                    update (IncrementN 5) 5
                        |> Tuple.first
                        |> Expect.equal 10
            ]
        , describe "n減るカウンタ"
            [ test "カウンタが5のとDecrementN 5されると0になる" <|
                \() ->
                    update (DecrementN 5) 5
                        |> Tuple.first
                        |> Expect.equal 0
            , test "カウンタが1のとDecrementN 3されると-2になる" <|
                \() ->
                    update (DecrementN 3) 1
                        |> Tuple.first
                        |> Expect.equal -2
            ]
        ]


viewTest : Test
viewTest =
    describe "viewのテスト" <|
        [ describe "カウンタの表示"
            [ test "カウンタは0を表示している" <|
                \() ->
                    view 0
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "0" ]
            , test "カウンタは15を表示している" <|
                \() ->
                    view 15
                        |> Query.fromHtml
                        |> Query.find [ tag "p" ]
                        |> Query.has [ text "15" ]
            ]
        , describe "増減ボタン"
            [ test "+ボタンはIncrement Msgを発行する" <|
                \() ->
                    view 0
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "+" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Increment
            , test "-ボタンはDecrement Msgを発行する" <|
                \() ->
                    view 0
                        |> Query.fromHtml
                        |> Query.find [ tag "button", containing [ text "-" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect Decrement
            ]
        ]
