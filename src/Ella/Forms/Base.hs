module Ella.Forms.Base where

-- Classes --
class HasId a where
    setId :: String -> a -> a
    getId :: a -> String
