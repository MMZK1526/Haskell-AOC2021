{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utilities where

import           Data.Either
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Gadgets.IO

listToPair [a, b] = (a, b)

readInput :: String -> IO Text
readInput path = 
  handleDNE (const $ T.readFile (path ++ ".txt")) $ T.readFile path

readInt :: Integral a => Text -> a
readInt = fst . fromRight undefined . T.signed T.decimal . T.stripStart

splitPair :: Text -> Text -> (Text, Text)
splitPair = (listToPair .) . T.splitOn
