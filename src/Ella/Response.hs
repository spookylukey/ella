module Ella.Response (-- * Response object
                      Response
                    , content
                    , headers
                    , status
                    , cookies
                    , HeaderName(HeaderName)
                    , Cookie(..)
                    -- * Building Response objects
                    , buildResponse
                    , addContent
                    , setStatus
                    , setHeader
                    , addCookie
                    , deleteCookie
                    , standardCookie
                    , expireCookie
                    -- * Starting points for Response objects
                    , textResponse
                    , utf8TextResponse
                    , htmlResponse
                    , utf8HtmlResponse
                    , textBasedResponse
                    , emptyResponse
                    , redirectResponse
                    -- * Using Response objects
                    , formatResponse
                    ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BS
import Data.List (intersperse)
import Ella.CGI.Header (Headers, HeaderName(HeaderName))
import Network.CGI (ContentType(ContentType), showContentType)
import Network.CGI.Cookie (Cookie(..), showCookie)
import Ella.GenUtils (apply)
import System.Time (ClockTime(..), toUTCTime)

-- | Represents an HTTP response
data Response = Response {
      content :: ByteString -- ^ The body of the response
    , headers :: Headers    -- ^ The HTTP headers of the response
    , status :: Int         -- ^ HTTP status code
    , cookies :: [Cookie]   -- ^ Cookies to be set.  Uses Cookie from Network.CGI.Cookie
    } deriving (Show, Eq)

--
-- * Creating responses
--

-- | A basic, empty 200 OK response
emptyResponse :: Response
emptyResponse = Response { content = BS.empty
                         , headers = []
                         , status = 200
                         , cookies = []
                         }

-- | Add a string to a response
addContent :: ByteString -> Response -> Response
addContent c resp = resp { content =  BS.append (content resp) c }

-- | Set the HTTP status code of a response
setStatus :: Int -> Response -> Response
setStatus s resp = resp { status = s }

-- | Set an HTTP header.  Previous values (if present) will be overwritten
setHeader :: String -> String -> Response -> Response
setHeader h val resp = let headername = HeaderName h
                           removed = filter ((/= headername) . fst) (headers resp)
                           updated = removed ++ [(headername, val)]
                       in resp { headers = updated }

-- | Add cookie to a response.  Cookie structure is from Network.CGI.Cookie
addCookie :: Cookie -> Response -> Response
addCookie cookie resp = resp { cookies = cookies resp ++ [cookie] }


standardCookie = Cookie { cookieName = ""
                        , cookieValue = ""
                        , cookieExpires = Nothing
                        , cookieDomain = Nothing
                        , cookiePath = Just "/"
                        , cookieSecure = False
                        }

expireCookie cookie = cookie { cookieExpires = Just $ toUTCTime $ TOD 1 0 }
oldCookie name = expireCookie $ standardCookie { cookieName = name }

-- | Delete the named cookie in the client
deleteCookie :: String -> Response -> Response
deleteCookie name resp = resp { cookies = cookies resp ++ [oldCookie name] }

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
contentTypeValue mimetype charset = mimetype ++ "; charset=" ++ charset
textContent charset = contentTypeValue "text/plain" charset
htmlContent charset = contentTypeValue "text/html" charset

-- | An empty response of the given mimetype and charset.
textBasedResponse :: String -> String -> Response
textBasedResponse mimetype charset = emptyResponse {
                                       headers = [(contentTypeName, contentTypeValue mimetype charset)]
                                     }

-- | An empty text/plain response of a given charset
textResponse :: String -> Response
textResponse charset = emptyResponse {
                         headers = [(contentTypeName, textContent charset)]
                       }

-- | An empty text/html response of a given charset
htmlResponse :: String -> Response
htmlResponse charset = emptyResponse {
                         headers = [(contentTypeName, htmlContent charset)]
                       }

-- | An empty UTF8 text/plain response.  The user is responsible
-- for ensuring that that content added to this response is actually
-- UTF8 ByteStrings.
utf8TextResponse :: Response
utf8TextResponse = textResponse "UTF-8"

-- | An empty UTF8 text/html response.  The user is responsible
-- for ensuring that that content added to this response is actually
-- UTF8 ByteStrings.
utf8HtmlResponse :: Response
utf8HtmlResponse = htmlResponse "UTF-8"

-- | Build a Response from a list of Response transformation functions
-- and an initial Response.
--
-- This is a convenient way of creating responses:
--
-- > resp = buildResponse [ setHeader "Location" foo
-- >                      , setStatus 302
-- >                      ] utf8HtmlResponse
buildResponse :: [Response -> Response] -> Response -> Response
buildResponse = apply

allHeaders resp =
    let statusHeader = (HeaderName "Status", show $ status resp)
        cookieHeaders = map (\c -> (HeaderName "Set-Cookie", showCookie c)) $ cookies resp
    in headers resp ++ cookieHeaders ++ [statusHeader]

-- | Convert a Response into the format needed for HTTP.
--
-- Copied from Network.CGI.Protocol, thank you Bjorn Bringert :-)
formatResponse :: Response -> ByteString
formatResponse resp =
    -- NOTE: we use CRLF since lighttpd mod_fastcgi can't handle
    -- just LF if there are CRs in the content.
    unlinesCrLf ([pack (n++": "++v) | (HeaderName n,v) <- allHeaders resp]
                ++ [BS.empty, content resp])
  where unlinesCrLf = BS.concat . intersperse (pack "\r\n")


-- | Create an HTTP 302 redirect
redirectResponse :: String -> Response
redirectResponse location =
    buildResponse [ setStatus 302
                  , setHeader "Location" location
                  ] emptyResponse
