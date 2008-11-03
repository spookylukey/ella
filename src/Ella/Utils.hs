module Ella.Utils ( addHtml
                 )

where

import Text.XHtml (renderHtml)
import Ella.Response (addContent)
import Ella.GenUtils (utf8)

-- Utility functions
addHtml html resp = addContent (utf8 $ renderHtml html) resp
