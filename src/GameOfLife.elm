module GameOfLife exposing (Cell, World, stepGen)

import Set exposing (Set)
import Dict
import Dict.Extra

type alias Cell = (Int, Int)

type alias World = Set Cell

perimeter : Cell -> List Cell
perimeter (x, y) = [
        (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
        (x - 1, y),                 (x + 1, y),
        (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    ]

isSurvivor : World -> Cell -> Bool
isSurvivor world cell =
    let
        nrOfNeighbours = cell |> perimeter |> Set.fromList |> Set.intersect world |> Set.size
    in
        nrOfNeighbours == 2 || nrOfNeighbours == 3

newBornWorld world =
    let
        zombiesAndAlives =
            world                 -- Set Cell
            |> Set.toList                           -- List Cell
            |> List.concatMap perimeter                         -- List Cell
            |> Dict.Extra.groupBy (\cell -> cell) -- Dict Cell   (List Cell)
            |> Dict.map     (\cell cells -> List.length cells)  -- Dict Cell Int
            |> Dict.filter  (\_ nr    -> nr == 3)               -- Dict Cell Int
            |> Dict.keys   -- List Cell
            |> Set.fromList -- List Cell
    in
        Set.diff zombiesAndAlives world

stepGen : World -> World
stepGen world =
    let
        survivors = world |> Set.filter (isSurvivor world)
        newBorns = newBornWorld world
    in
        Set.union survivors newBorns
