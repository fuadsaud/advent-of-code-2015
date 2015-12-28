module Day5 where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Text.Regex.TDFA

type Rule = String -> Maybe String

inputFileName :: String
inputFileName = "input"

pairs :: [a] -> [(a, a)]
pairs lst = zip lst (tail lst)

isIn :: Eq a => [a] -> a -> Bool
isIn = flip elem

fromBool :: Bool -> a -> Maybe a
fromBool b = if b then Just else const Nothing

homogeneousTuple = ((==) <$> fst <*> snd)

threeVowels :: Rule
threeVowels str = fromBool ((atLeast 3 . vowels) str) str
  where
      atLeast n = (>= n) . length
      vowels = filter (isIn "aeiou")

oneDouble :: Rule
oneDouble str = (find homogeneousTuple . pairs) str >>= const (Just str)

noSeqs :: Rule
noSeqs str = fromBool ((isNothing . find (isIn disallowedPairs) . pairs) str) str
  where
      disallowedPairs = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]

pairTwice :: Rule
pairTwice str = str =~ "([a-z][a-z]).*(\1)"

repeatsBetween :: Rule
repeatsBetween str = Nothing

nice :: String -> Bool
nice str = isJust (threeVowels str >>= oneDouble >>= noSeqs)

main :: IO ()
main = do
    input <- readFile inputFileName

    print . length . filter nice . lines . map toLower $ input
