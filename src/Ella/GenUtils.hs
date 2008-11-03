-- | General utility functions that do not depend on other functions
-- in Web modules
module Ella.GenUtils

where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import GHC.Exts( IsString(..) )
instance IsString ByteString where
    fromString = UTF8.fromString

utf8 = UTF8.fromString

-- | Apply a list of transformation functions to an object
apply :: [a -> a] -- ^ List of functions
      -> a        -- ^ Initial value
      -> a
apply fs init = foldl (flip ($)) init fs

-- | Same as apply with arguments flipped
with = flip apply
