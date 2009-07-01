{-# LANGUAGE OverloadedStrings #-}
module Ella.TestUtils where

import Ella.Framework
import Ella.GenUtils (utf8)
import Ella.Request
import Network.CGI (formEncode)
import qualified Data.ByteString.Lazy as BS


mkGetReq path = mkRequest [("REQUEST_METHOD", "GET")
                          ,("PATH_INFO", path)
                          ,("REQUEST_URI", escapePathWithEnc path utf8Encoding)
                          ] "" utf8Encoding

mkPostReq path postData =
    -- TODO - handle non ASCII data in postData -- formEncode doesn't
    -- do this.
    let encodedPostData = utf8 $ formEncode postData
    in mkRequest [ ("REQUEST_METHOD", "POST")
                 , ("PATH_INFO", path)
                 , ("CONTENT_TYPE", "application/x-www-form-urlencoded")
                 , ("CONTENT_LENGTH", show $ BS.length encodedPostData)
                 ] encodedPostData utf8Encoding

-- | Add cookie name/value pairs to a request
addCookieValues cookies req = req { allCookies = (allCookies req) ++ cookies }
