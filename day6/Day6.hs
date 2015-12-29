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

turnOn :: Int -> Int
turnOn = const  1

turnOff :: Int -> Int
turnOff = const 0

toggle :: Int -> Int
toggle 0 = 1
toggle 1 = 0

increaseBrightness :: Int -> Int
increaseBrightness = (+1)

decreaseBrightness :: Int -> Int
decreaseBrightness x =  if (x - 1) >= 0
                          then x - 1
                          else 0

doubleIncreaseBrightness :: Int -> Int
doubleIncreaseBrightness = (+2)

pointBetween :: Point -> Point -> Point -> Bool
pointBetween (x1, y1) (x2, y2) (x3, y3) = x1 <= x3 && x3 <= x2 && y1 <= y3 && y3 <= y2

swapElement :: (Int -> Int) -> Point -> Point -> Point -> Int -> Int
swapElement swap p1 p2 p3 original = if pointBetween p1 p2 p3
                                       then swap original
                                       else original

subGridTransformation :: (Int -> Int) -> Point -> Point -> GridTransformation
subGridTransformation f p1 p2 = mapWithIndex (swapElement f p1 p2)

onOffTransformation :: Instruction -> GridTransformation
onOffTransformation (TurnOn p1 p2)  = subGridTransformation turnOn p1 p2
onOffTransformation (TurnOff p1 p2) = subGridTransformation turnOff p1 p2
onOffTransformation (Toggle p1 p2)  = subGridTransformation toggle p1 p2

intensityTransformation :: Instruction -> GridTransformation
intensityTransformation (TurnOn p1 p2)  = subGridTransformation increaseBrightness p1 p2
intensityTransformation (TurnOff p1 p2) = subGridTransformation decreaseBrightness p1 p2
intensityTransformation (Toggle p1 p2)  = subGridTransformation doubleIncreaseBrightness p1 p2

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
