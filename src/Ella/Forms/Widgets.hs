{-# LANGUAGE DisambiguateRecordFields #-}

module Ella.Forms.Widgets where

import Ella.Forms.Base
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )

-- data types, defined in separate modules because GHC can't handle it
-- otherwise (even with DisambiguateRecordFields).
import qualified Ella.Forms.Widgets.TextInput as TextInput
import qualified Ella.Forms.Widgets.Textarea as Textarea


-- Labels

data Label a = Label {
      text :: X.Html
    , control :: Maybe a
    }


instance (HasId a) => X.HTML (Label a) where
    toHtml l = let attrs = case control l of
                             Nothing -> []
                             Just c -> case getId c of
                                         "" -> []
                                         theid -> [ X.thefor theid ]
               in X.label ! attrs << text l

makeLabel :: (HasId a) => String -> a -> Label a
makeLabel t c = Label { text = X.toHtml t
                      , control = Just c
                      }
