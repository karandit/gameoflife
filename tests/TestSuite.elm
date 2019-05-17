module TestSuite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Set exposing (empty)
import GameOfLife exposing (..)

suite : Test
suite =
    describe "Any cell "
            [ test "that is live with fewer than two live neighbours dies" <|
                \_ ->
                        twoCellWorld
                        |> GameOfLife.stepGen
                        |> Expect.all [
                            (Set.member (0, 0) >> Expect.false "Expected the (0, 0) is dead"),
                            (Set.member (0, 1) >> Expect.false "Expected the (0, 1) is dead")
                        ]
            ,  test " in the universe dies due to underpopulation" <|
                \_ ->
                    threeLoneWorld
                    |> GameOfLife.stepGen
                    |> Expect.all [
                            (Set.member (0, 0) >> Expect.false "Expected the (0, 0) is dead"),
                            (Set.member (3, 0) >> Expect.false "Expected the (0, 1) is dead"),
                            (Set.member (2, 2) >> Expect.false "Expected the (0, 1) is dead")
                    ]
            , test " that is live with two or three live neighbours lives on to the next generation" <|
                \_ ->
                    threeDiagonalWorld
                    |> GameOfLife.stepGen
                    |> Expect.all [
                            (Set.member (1, 1) >> Expect.true "Expected the (1, 1) is alive"),
                            (Set.member (0, 0) >> Expect.false "Expected the (0, 1) is dead"),
                            (Set.member (0, 1) >> Expect.false "Expected the (0, 1) is dead")
                    ]
            , test " that is live with more than three live neighbours dies" <|
                \_ ->
                    horseShoeWorld
                    |> GameOfLife.stepGen
                    |> Set.member (1, 1)
                    |> Expect.false "Expected the (1, 1) is dead"
            , test "  that is dead with exactly three live neighbours becomes a live cell" <|
                \_ ->
                    triadWorld
                    |> GameOfLife.stepGen
                    |> Set.member (1, 1)
                    |> Expect.true "Expected the (1, 1) is resurrected"
            ]

twoCellWorld = [(0, 0), (0, 1)] |> Set.fromList
threeLoneWorld = [(0, 0), (3, 0), (2, 2)] |> Set.fromList
threeDiagonalWorld = [(0, 0), (1, 1), (2,2)] |> Set.fromList
horseShoeWorld = [(0, 0), (0, 1), (1,1), (2,0), (2,1)] |> Set.fromList
triadWorld = [(0, 0), (0, 2), (2, 2)] |> Set.fromList
