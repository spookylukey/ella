{-# LANGUAGE DisambiguateRecordFields, MultiParamTypeClasses, TypeSynonymInstances #-}
module Ella.Forms.Widgets.TextInput where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (nullToNothing)
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )

data TextInput = TextInput {
      value :: String
    , name :: String
    , identifier :: String
    , maxlength :: Maybe Int
    , size :: Maybe Int
    , password :: Bool
    }


emptyTextInput = TextInput { value = ""
                           , maxlength = Nothing
                           , size = Nothing
                           , name = ""
                           , identifier = ""
                           , password = False
                           }

instance X.HTML TextInput where
    toHtml t = let attrs =  [ X.thetype (if password t then "password" else "text")
                            , X.name $ name t
                            , X.value $ value t
                            ] ++
                              catMaybes [ liftM X.identifier $ nullToNothing $ identifier t
                                        , liftM X.maxlength $ maxlength t
                                        , liftM (X.size . show) $ size t
                                        ]
                    in X.input ! attrs

instance HasId TextInput where
    setId theid t = t { identifier = theid }
    getId t = identifier t

instance HasVal TextInput String where
    setVal val t = t { value = val }
    getVal t = value t
