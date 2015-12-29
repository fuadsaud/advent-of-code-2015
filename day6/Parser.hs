module Parser where

import Data.List.Split

type Point = (Int, Int)

data Instruction = TurnOn Point Point |
                   TurnOff Point Point |
                   Toggle Point Point deriving (Show)

makeInstruction f ((TInt x1) : (TInt y1) : (TInt x2) : (TInt y2) : []) = f (x1, y1) (x2, y2)

data Token = TOn | TOff | TToggle | TInt Int deriving (Show)

tokenize :: [String] -> [Token]
tokenize ("turn"    : xs) = tokenize xs
tokenize ("through" : xs) = tokenize xs
tokenize ("on"      : xs) = TOn:(tokenize xs)
tokenize ("off"     : xs) = TOff:(tokenize xs)
tokenize ("toggle"  : xs) = TToggle:(tokenize xs)
tokenize (x : xs) = (map (TInt . read) . splitOn "," $ x) ++ (tokenize xs)
tokenize [] = []

parse :: [Token] -> Instruction
parse (TOn : xys) = makeInstruction TurnOn xys
parse (TOff : xys) = makeInstruction TurnOff xys
parse (TToggle : xys) = makeInstruction Toggle xys
