module Day3 where

import Data.Function
import Data.List
import qualified Data.Set as Set (fromList, size)

type Token = Char
type House = (Integer, Integer)
type Step = (House -> House)

inputFileName :: FilePath
inputFileName = "input"

origin :: House
origin = (0, 0)

countUniq :: Ord a => [a] -> Integer
countUniq = toInteger . Set.size . Set.fromList

step :: Token -> Step
step '<' (x, y) = (x - 1, y)
step '^' (x, y) = (x, y + 1)
step '>' (x, y) = (x + 1, y)
step 'v' (x, y) = (x, y - 1)
step c h = h

split xs = (odds xs, evens xs)
  where
    odds (x:xs) = x : evens xs
    odds xs     = []
    evens xs    = odds (drop 1 xs)

deliveries :: [Step] -> [House]
deliveries = scanl (&) (0, 0)

countSantaHouses :: [Step] -> Integer
countSantaHouses = countUniq . deliveries

countRoboSantaHouses :: [Step] -> Integer
countRoboSantaHouses steps = countUniq (deliveries santaSteps ++ deliveries roboSteps)
  where
      (santaSteps, roboSteps) = split steps
      -- santaSteps = filter (\x -> )even dels
      -- roboSteps = findIndices odd dels

main = do
    input <- readFile inputFileName

    let steps = map step input

    print $ "Santa alone delivered presents to " ++ show (countSantaHouses steps) ++ " houses"
    print $ "Santa + RoboSanta delivered presents to " ++ show (countRoboSantaHouses steps) ++ " houses"
