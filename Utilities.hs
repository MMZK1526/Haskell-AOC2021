module Utilities where

import           Data.Either
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Gadgets.IO

readInput :: String -> IO Text
readInput path = 
  handleDNE (const $ T.readFile (path ++ ".txt")) $ T.readFile path

readInt :: Text -> Integer
readInt = fst . fromRight undefined . T.signed T.decimal . T.stripStart
