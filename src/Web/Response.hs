module Web.Response (Response,
                     content,
                     headers,
                     addContent,
                     textResponse,
                     utf8TextResponse,
                     htmlResponse,
                     utf8HtmlResponse,
                     emptyResponse,
                     formatResponse,
                     setStatus,
                     buildResponse) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Network.CGI.Protocol (Headers, HeaderName(HeaderName))
import Network.CGI (ContentType(ContentType), showContentType)

data Response = Response {
      content :: ByteString
    , headers :: Headers
    , status :: Int
    } deriving (Show, Eq)

--
-- * Creating responses
--

emptyResponse = Response { content = BS.empty
                         , headers = []
                         , status = 200 
                         }

addContent :: ByteString -> Response -> Response
addContent c resp = resp { content =  BS.append (content resp) c }

setStatus :: Int -> Response -> Response
setStatus s resp = resp { status = s }

---
--- * Shortcuts for common defaults
---

{-
TODO
 - add utility functions for writing HTML
 - add encoding/charset to response, so that it can automatically
   convert HTML to the correct encoding.
-}

contentTypeName = HeaderName "Content-type"
textContent charset = "text/plain; charset=" ++ charset
htmlContent charset = "text/html; charset=" ++ charset

textResponse charset = emptyResponse {
                         headers = [(contentTypeName, textContent charset)]
                       }

htmlResponse charset = emptyResponse {
                         headers = [(contentTypeName, htmlContent charset)]
                       }

utf8TextResponse = textResponse "UTF-8"

-- | Create an empty response for sending HTML, UTF-8 encoding
utf8HtmlResponse = htmlResponse "UTF-8"

-- | Build a Response from a list of Response transformation functions
-- and an initial Response
buildResponse :: [Response -> Response] -> Response -> Response
buildResponse fs rinit = foldl (flip ($)) rinit fs

allHeaders resp =
    let statusHeader = (HeaderName "Status", show $ status resp)
    in headers resp ++ [statusHeader]

-- | Convert a Response into the foratm needed for HTTP
-- Copied from Network.CGI.Protocol, thank you Bjorn Bringert :-)
formatResponse :: Response -> ByteString
formatResponse resp =
    -- NOTE: we use CRLF since lighttpd mod_fastcgi can't handle
    -- just LF if there are CRs in the content.
    unlinesCrLf ([BS.pack (n++": "++v) | (HeaderName n,v) <- allHeaders resp]
                ++ [BS.empty, content resp])
  where unlinesCrLf = BS.concat . intersperse (BS.pack "\r\n")
