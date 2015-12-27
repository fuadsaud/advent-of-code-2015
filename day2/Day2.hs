module Day2 where

import Data.List.Split (splitOn)
import Data.List (sort)

data Box = Box Integer Integer Integer deriving Show

makeBox (l:w:h:[]) = Box l w h

dimensions (Box l w h) = [l, w, h]

inputFileName = "input"

instance Read Box where
    readsPrec _ r = [(makeBox . map read . splitOn "x" $ r, "")]

surface :: Box -> Integer
surface (Box l w h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

wrappingPaperArea :: Box -> Integer
wrappingPaperArea box@(Box l w h) = smallerSize + surface box
  where
    smallerSize = minimum [l * w, w * h, h * l]

ribbonArea :: Box -> Integer
ribbonArea box = ribbonWrapArea + ribbonBowArea
  where
      dims = dimensions box
      ribbonWrapArea = sum . map (*2) . take 2 . sort $ dims
      ribbonBowArea = product dims

totalWrappingPaperArea :: [Box] -> Integer
totalWrappingPaperArea = sum . map wrappingPaperArea

totalRibbonArea :: [Box] -> Integer
totalRibbonArea = sum . map ribbonArea

main = do
    input <- readFile inputFileName

    let boxes = map read . lines $ input

    print $ totalWrappingPaperArea boxes
    print $ totalRibbonArea boxes
