-- | General utility functions that do not depend on other functions
-- in Web modules
module Ella.GenUtils

where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import GHC.Exts( IsString(..) )

-- | Takes a String and returns UTF8 ByteString
utf8 :: String -> ByteString
utf8 = UTF8.fromString

-- | Apply a list of transformation functions to a value
apply :: [a -> a] -- ^ List of functions
      -> a        -- ^ Initial value
      -> a
apply = flip with

-- | Same as apply with arguments flipped
with :: a -> [a -> a] -> a
with = foldl (flip ($))

-- | Parse a value, or fail if the entire string cannot be parsed
--
-- Note that this relies on 'reads', which appears to be buggy for Int
-- -- it works something like parsing as 'Integer', then wrapping to a 32 bit
-- Int.  The 'Param' instance for 'Int' has fixed this, so you can
-- use 'capture' instead.
exactParse :: (Read a, Monad m) => String -> m a
exactParse s =
  case reads s of
    [(val, "")] -> return val
    otherwise -> fail ("Cannot parse '" ++ s ++ "'")

-- | Converts empty string to Nothing, everything else wrapped in Just
nullToNothing :: String -> Maybe String
nullToNothing "" = Nothing
nullToNothing x  = Just x
