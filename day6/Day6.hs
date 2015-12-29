module Day6 where

import Parser
import Data.Function
import Data.List.Split
import Numeric.Matrix (Matrix, mapWithIndex, toList, zero)

type Grid = Matrix Int
type GridTransformation = Grid -> Grid

makePoint [x : y : xys] = (x, y)

initialGrid :: Grid
initialGrid = zero 1000

toggleInt :: Int -> Int
toggleInt 0 = 1
toggleInt 1 = 0

pointBetween :: Point -> Point -> Point -> Bool
pointBetween (x1, y1) (x2, y2) (x3, y3) = x1 <= x3 && x3 <= x2 && y1 <= y3 && y3 <= y2

swapElement :: (Int -> Int) -> Point -> Point -> Point -> Int -> Int
swapElement swap p1 p2 p3 original =
    if pointBetween p1 p2 p3
      then swap original
      else original

turnOn :: Point -> Point -> GridTransformation
turnOn p1 p2 = mapWithIndex (swapElement (const 1) p1 p2)

turnOff :: Point -> Point -> GridTransformation
turnOff p1 p2 = mapWithIndex (swapElement (const 0) p1 p2)

toggle :: Point -> Point -> GridTransformation
toggle p1 p2 = mapWithIndex (swapElement toggleInt p1 p2)

transformation :: Instruction -> GridTransformation
transformation (TurnOn p1 p2) = turnOn p1 p2
transformation (TurnOff p1 p2) = turnOff p1 p2
transformation (Toggle p1 p2) = toggle p1 p2

compute :: String -> GridTransformation
compute = transformation .  parse . tokenize . splitOn " "

applyTransformations :: Grid -> [GridTransformation] -> Grid
applyTransformations = foldl (&)

countLightsOn :: Grid -> Int
countLightsOn = length . filter (==1) . concat . toList

main :: IO ()
main = do
    input <- readFile "input"

    let transformations = map compute . lines $ input

    print . countLightsOn . applyTransformations initialGrid $ transformations
