module Day4 where

import Data.List
import Control.Monad
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

type Message = BS.ByteString
type Digest = BS.ByteString

secret :: String
secret = "ckczppom"

md5 :: Message -> Digest
md5 = MD5.hash

startsWithZeroes :: Int -> Digest -> Bool
startsWithZeroes n = BS.isPrefixOf (C8.pack $ replicate n '0') . B16.encode

message :: String -> Int -> Message
message key i = C8.pack $ key ++ show i

digests :: [Digest]
digests = map (md5 . message secret) [1..]

untilSolution :: Int -> Int
untilSolution nZeroes = until (startsWithZeroes nZeroes . md5 . message secret) (+1) 1

findSolution :: Int -> Maybe Int
findSolution nZeroes = fmap (+1) . findIndex (startsWithZeroes nZeroes) $ digests

main :: IO ()
main = do
    -- forM_ [5, 6] $ \i -> print $ untilSolution i
    forM_ [5, 6] $ \i -> print $ findSolution i
