{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utilities where

import           Data.Char
import           Data.Either
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Gadgets.IO
import qualified Numeric as N

listToPair :: [b] -> (b, b)
listToPair [a, b] = (a, b)

readInput :: String -> IO Text
readInput path = 
  handleDNE (const $ T.readFile (path ++ ".txt")) $ T.readFile path

readInt :: Integral a => Text -> a
readInt = fst . fromRight undefined . T.signed T.decimal . T.stripStart

splitPair :: Text -> Text -> (Text, Text)
splitPair = (listToPair .) . T.splitOn

binToDec :: Integral i => String -> i
binToDec = foldl ((. (fromIntegral . digitToInt)) . (+) . (2 *)) 0

hexToBin :: String -> String
hexToBin = flip (N.showIntAtBase 2 intToDigit . fst . head . N.readHex) ""
