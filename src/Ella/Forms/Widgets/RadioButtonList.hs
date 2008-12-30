{-# LANGUAGE DisambiguateRecordFields, MultiParamTypeClasses, TypeSynonymInstances #-}
module Ella.Forms.Widgets.RadioButtonList where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (nullToNothing)
import qualified Text.XHtml as X
import qualified Ella.Forms.Widgets.RadioButton as RB
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )


data RadioButtonList = RadioButtonList {
      value :: String
    , name :: String
    , identifier :: String
    , values :: [String]
    , captions :: [X.Html]
}

instance X.HTML RadioButtonList where
    toHtml rbl = X.toHtml $
                 do
                   (val, caption, idx) <- zip3 (values rbl) (captions rbl) [0..]
                   return (X.label << (RB.RadioButton { value = val
                                                      , name = name rbl
                                                      , identifier = if (not . null . identifier) rbl
                                                                     then identifier rbl ++ "_" ++ show idx
                                                                     else ""
                                                      , checked = val == value rbl
                                                      }
                                       +++ " " +++ caption)
                           +++ X.br)

instance HasId RadioButtonList where
    setId theid t = t { identifier = theid }
    getId t = identifier t

instance HasVal RadioButtonList String where
    setVal val t = t { value = val }
    getVal t = value t

