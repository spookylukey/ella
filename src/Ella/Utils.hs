module Ella.Utils ( addHtml
                 )

where

import Text.XHtml (renderHtml, HTML)
import Ella.Response (addContent, Response)
import Ella.GenUtils (utf8)

-- Utility functions

-- | Adds HTML to a response
addHtml :: (HTML html) => html -> Response -> Response
addHtml html resp = addContent (utf8 $ renderHtml html) resp
