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

increaseBrightness :: Point -> Point -> GridTransformation
increaseBrightness p1 p2 = mapWithIndex (swapElement (+1) p1 p2)

decreaseBrightness :: Point -> Point -> GridTransformation
decreaseBrightness p1 p2 = mapWithIndex (swapElement safeDecrement p1 p2)
  where
      safeDecrement x = if (x - 1) >= 0
                        then x - 1
                        else 0

doubleIncreaseBrightness :: Point -> Point -> GridTransformation
doubleIncreaseBrightness p1 p2 = mapWithIndex (swapElement (+2) p1 p2)

onOffTransformation :: Instruction -> GridTransformation
onOffTransformation (TurnOn p1 p2) = turnOn p1 p2
onOffTransformation (TurnOff p1 p2) = turnOff p1 p2
onOffTransformation (Toggle p1 p2) = toggle p1 p2

intensityTransformation :: Instruction -> GridTransformation
intensityTransformation (TurnOn p1 p2) = increaseBrightness p1 p2
intensityTransformation (TurnOff p1 p2) = decreaseBrightness p1 p2
intensityTransformation (Toggle p1 p2) = doubleIncreaseBrightness p1 p2

compute :: (Instruction -> GridTransformation) -> String -> GridTransformation
compute transformation = transformation .  parse . tokenize . splitOn " "

applyTransformations :: Grid -> [GridTransformation] -> Grid
applyTransformations = foldl (&)

countLightsOn :: Grid -> Int
countLightsOn = length . filter (==1) . concat . toList

totalLightsIntensity :: Grid -> Int
totalLightsIntensity = sum . concat . toList

main :: IO ()
main = do
    input <- readFile "input"

    let onOffTransformations = map (compute onOffTransformation) . lines $ input
    let intensityTransformations = map (compute intensityTransformation) . lines $ input

    -- print . countLightsOn . applyTransformations initialGrid $ onOffTransformations
    print . totalLightsIntensity . applyTransformations initialGrid $ intensityTransformations
