module Ella.Forms.Widgets where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )

-- Classes --
class HasId a where
    setId :: String -> a -> a
    getId :: a -> Maybe String
    removeId :: a -> a


-- Widgets --
data TextInput = TextInput {
      defaultVal :: String
    , maxlength :: Maybe Int
    , size :: Maybe Int
    , name :: String
    , identifier :: Maybe String
    }


emptyTextInput = TextInput { defaultVal = ""
                           , maxlength = Nothing
                           , size = Nothing
                           , name = ""
                           , identifier = Nothing
                           }

data Label a = Label {
      text :: X.Html
    , control :: Maybe a
    }

-- HTML instances --

instance X.HTML TextInput where
    toHtml t = let attrs =  [ X.thetype "text"
                            , X.name $ name t
                            , X.value $ defaultVal t ] ++
                          catMaybes [ liftM X.identifier $ identifier t
                                    , liftM X.maxlength $ maxlength t
                                    , liftM (X.size . show) $ size t
                                    ]
               in X.input ! attrs

instance (HasId a) => X.HTML (Label a) where
    toHtml l = let attrs = case control l of
                             Nothing -> []
                             Just c -> case getId c of
                                         Nothing -> []
                                         Just theid -> [ X.thefor theid ]
               in X.label ! attrs << text l

-- HasId instances

instance HasId TextInput where
    setId theid t = t { identifier = Just theid }
    getId t = identifier t
    removeId t = t { identifier = Nothing }


-- Utilities
makeLabel :: (HasId a) => String -> a -> Label a
makeLabel t c = Label { text = X.toHtml t
                      , control = Just c
                      }
