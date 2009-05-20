module Ella.Utils ( addHtml
                 )

where

import Ella.GenUtils (utf8)
import Ella.Response (addContent, Response)
import Text.XHtml (renderHtml, HTML)

-- Utility functions

-- | Adds HTML to a response
addHtml :: (HTML html) => html -> Response -> Response
addHtml html resp = addContent (utf8 $ renderHtml html) resp
