module Day1 where

import Data.List

type Token = Char
type Instruction = (Integer -> Integer)
type Script = [Instruction]

inputFileName = "input"

computeFloor :: Token -> Instruction
computeFloor '(' = (+ 1)
computeFloor ')' = subtract 1
computeFloor c = id

reduceFloor :: Integer -> Instruction -> Integer
reduceFloor = flip ($)

destinationFloor :: Script -> Integer
destinationFloor = foldl reduceFloor 0

entersBasementAt :: Script -> Integer
entersBasementAt = genericLength . takeWhile (>= 0) . floorSequence
  where
    floorSequence = reverse . foldl (\acc f -> (f $ head acc):acc) [0]

main = do
    input <- readFile inputFileName

    let script = map computeFloor input

    print $ "Destination floor: " ++ show (destinationFloor script)
    print $ "Enters basement at step no " ++ show (entersBasementAt script)
