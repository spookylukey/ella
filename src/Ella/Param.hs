{-# OPTIONS_GHC -XTypeSynonymInstances #-}
module Ella.Param ( Param(..)
                  )

where

import Ella.GenUtils (exactParse)

-- | Type class representing parameters that can be captured (usually
-- | from URL components)
class Param a where
    -- | Convert a string to a value, returning Nothing if not possible
    capture :: String -> Maybe a

instance Param Int where
    capture = exactParse

instance Param Integer where
    capture = exactParse

instance Param String where
    capture = Just
