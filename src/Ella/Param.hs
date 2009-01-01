{-# OPTIONS_GHC -XTypeSynonymInstances #-}
-- | Type class and utilities used for making it easy to capture
-- parameters from URLs and parse GET/POST values as specific types.
module Ella.Param ( Param(..)
                  , parseOrDefault
                  , captureOrDefault
                  )

where

import Maybe (fromMaybe)
import Ella.GenUtils (exactParse)

-- | Type class representing parameters that can be captured (usually
-- from URL components)
class Param a where
    -- | Convert a string to a value, returning Nothing if not possible
    capture :: String -> Maybe a

instance Param Int where
    capture = exactParse

instance Param Integer where
    capture = exactParse

instance Param String where
    capture = Just

-- | Parse a value (packed in a Just) or return a default
--
-- This is useful in dealing with 'Maybe' vals returned from getGET,
-- getPOST etc.  It can be used with any types that are instances of Read.
-- Note that 'Read' for Strings expects quotes and escaping, so this
-- in general is not useful for String values.
parseOrDefault :: (Read a) => Maybe String -> a -> a
parseOrDefault v d = fromMaybe d (v >>= exactParse)

-- | Parse a value (packed in a Just) or return a default
--
-- This is useful in dealing with 'Maybe' vals returned from getGET,
-- getPOST etc.  It can be used with any types that are instances of Param.
captureOrDefault :: (Param a) => Maybe String -> a -> a
captureOrDefault v d = fromMaybe d (v >>= capture)