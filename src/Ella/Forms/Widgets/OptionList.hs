{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Ella.Forms.Widgets.OptionList where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (nullToNothing)
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (!)
                  )

data OptionList = OptionList {
      selectedValues :: [String]
    , name :: String
    , identifier :: String
    , values :: [String]
    , captions :: [String]
    , multiple :: Bool
    , size :: Int
}

instance X.HTML OptionList where
    toHtml ol = let attrs = [ X.name $ name ol
                            , X.size $ show $ size ol
                            ] ++ catMaybes
                            [ liftM X.identifier $ nullToNothing $ identifier ol
                            , if multiple ol then Just $ X.multiple else Nothing
                            ]
                in X.select ! attrs
                       << (X.toHtml $ do
                             (val, caption) <- zip (values ol) (captions ol)
                             let optattrs = [ X.value val ] ++
                                            if val `elem` selectedValues ol
                                            then [ X.selected ]
                                            else [ ]
                             return $ X.option ! optattrs << (X.toHtml caption)
                          )

instance HasId OptionList where
    setId theid t = t { identifier = theid }
    getId t = identifier t

instance HasVal OptionList [String] where
    setVal val t = t { selectedValues = val }
    getVal t = selectedValues t
