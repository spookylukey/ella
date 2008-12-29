{-# LANGUAGE MultiParamTypeClasses #-}
module Ella.Forms.Base where

-- Classes --
class HasId a where
    setId :: String -> a -> a
    getId :: a -> String

class HasVal a v where
    setVal :: v -> a -> a
    getVal :: a -> v
