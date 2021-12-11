{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Gadgets.Set where

import           Data.Set (Set)
import qualified Data.Set as S

pattern Empty :: Set a
pattern Empty <- (S.null -> True)
  where
    Empty = S.empty
