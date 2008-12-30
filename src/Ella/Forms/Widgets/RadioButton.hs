{-# LANGUAGE DisambiguateRecordFields, MultiParamTypeClasses, TypeSynonymInstances #-}
module Ella.Forms.Widgets.RadioButton where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (nullToNothing)
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )


data RadioButton = RadioButton {
      value :: String
    , name :: String
    , identifier :: String
    , checked :: Bool
}

instance X.HTML RadioButton where
    toHtml rb = let attrs = [ X.thetype "radio"
                            , X.name $ name rb
                            , X.value $ value rb
                            ] ++ catMaybes [ liftM X.identifier $ nullToNothing $ identifier rb
                                           , if checked rb then Just $ X.checked else Nothing
                                           ]
                in X.input ! attrs

instance HasId RadioButton where
    setId theid t = t { identifier = theid }
    getId t = identifier t

instance HasVal RadioButton String where
    setVal val t = t { value = val }
    getVal t = value t

