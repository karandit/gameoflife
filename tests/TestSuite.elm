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
                    |> Set.fromList
                    |> GameOfLife.stepGen
                    |> Expect.equal Set.empty
            ,  test " in the universe dies due to underpopulation" <|
                \_ ->
                    threeLoneWorld
                    |> Set.fromList
                    |> GameOfLife.stepGen
                    |> Expect.equal Set.empty
            , test " that is live with two or three live neighbours lives on to the next generation" <|
                \_ ->
                    threeDiagonalWorld
                    |> Set.fromList
                    |> GameOfLife.stepGen
                    |> Expect.equal (Set.singleton (1, 1))
            , test " that is live with more than three live neighbours dies" <|
                \_ ->
                    horseShoeWorld
                    |> Set.fromList
                    |> GameOfLife.stepGen
                    |> Set.member (1, 1)
                    |> Expect.false "Expected the (1, 1) is dead"
            , test "  that is dead with exactly three live neighbours becomes a live cell" <|
                \_ ->
                    triadWorld
                    |> Set.fromList
                    |> GameOfLife.stepGen
                    |> Set.member (1, 1)
                    |> Expect.true "Expected the (1, 1) is resurrected"
            ]

threeLoneWorld = [(0, 0), (3, 0), (2, 2)]

twoCellWorld = [(0, 0), (0, 1)]
threeDiagonalWorld = [(0, 0), (1, 1), (2,2)]
horseShoeWorld = [(0, 0), (0, 1), (1,1), (2,0), (2,1)]
triadWorld = [(0, 0), (0, 2), (2, 2)]
