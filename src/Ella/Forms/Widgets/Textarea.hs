{-# LANGUAGE DisambiguateRecordFields #-}
module Ella.Forms.Widgets.Textarea where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Ella.Forms.Base
import Ella.GenUtils (nullToNothing)
import qualified Text.XHtml as X
import Text.XHtml ( (<<)
                  , (+++)
                  , (!)
                  )

data Textarea = Textarea {
      value :: String
    , name :: String
    , identifier :: String
    , rows :: Maybe Int
    , cols :: Maybe Int
    }

emptyTextarea = Textarea {
                  value = ""
                , name = ""
                , identifier = ""
                , rows = Nothing
                , cols = Nothing
                }

instance X.HTML Textarea where
    toHtml t = let attrs = [ X.name $ name t
                           ] ++
                         catMaybes [ liftM X.identifier $ nullToNothing $ identifier t
                                   , liftM (X.cols . show) $ cols t
                                   , liftM (X.rows . show) $ rows t
                                   ]
               in X.textarea ! attrs << (value t)
