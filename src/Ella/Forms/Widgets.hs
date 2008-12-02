module Ella.Forms.Widgets where

import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )

data TextInput = TextInput {
      defaultVal :: String
    , maxlength :: Maybe Int
    , size :: Maybe Int
    }

emptyTextInput = TextInput { defaultVal = ""
                           , maxlength = Nothing
                           , size = Nothing
                           }

instance X.HTML TextInput where
    toHtml t = let attrs =  [ X.thetype "text"
                            , X.value $ defaultVal t ] ++
                          case maxlength t of
                            Nothing -> []
                            Just ml -> [X.maxlength ml]
                           ++
                          case size t of
                            Nothing -> []
                            Just s -> [X.size $ show s]
               in X.input ! attrs

