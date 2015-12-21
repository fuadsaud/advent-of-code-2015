module Day2 where

import Data.List.Split

data Box = Box Integer Integer Integer deriving Show

makeBox (l:w:h:[]) = Box l w h

inputFileName = "input"

instance Read Box where
    readsPrec _ r = [(makeBox . map read . splitOn "x" $ r, "")]

surface :: Box -> Integer
surface (Box l w h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

wrappingPaperSize :: Box -> Integer
wrappingPaperSize box@(Box l w h) = smallerSize + surface box
  where
    smallerSize = minimum [l * w, w * h, h * l]

totalArea :: [Box] -> Integer
totalArea = foldl (+) 0 . map wrappingPaperSize

main = do
    input <- readFile inputFileName

    let boxes = map read . lines $ input

    print $ totalArea boxes
