{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Request

where

import qualified Data.ByteString.Lazy.Char8 as BS
import Ella.Request
import Test.HUnit
import Ella.GenUtils () -- for IsString instance

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "" utf8Encoding)
testPath = "foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "" utf8Encoding)
testPathMissing = "" ~=? pathInfo (mkRequest [] "" utf8Encoding)
testPathUtf8 = "\233" ~=? pathInfo (mkRequest [("PATH_INFO", "\195\169")] "" utf8Encoding)
testRequestUriRaw = Just "/root/foo/%C3%A9/" ~=? requestUriRaw (mkRequest [("REQUEST_URI","/root/foo/%C3%A9/")
                                                                          ,("PATH_INFO","/foo/\195\169/")] "" utf8Encoding)

-- application/x-www-form-urlencoded
pr1_content = "foo=bar&baz=fizz&foo=bar2"
postRequest1 = mkRequest [ ("REQUEST_METHOD", "POST")
                         , ("CONTENT_TYPE", "application/x-www-form-urlencoded")
                         , ("CONTENT_LENGTH", show $ BS.length pr1_content)
                         ] pr1_content utf8Encoding

-- multipart/form-data
pr2_content = "------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"foo\"\r\n\r\nbar\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"baz\"\r\n\r\nfizz\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"foo\"\r\n\r\nbar2\r\n------------------------------6d9817ad0e6b--\r\n"
postRequest2 = mkRequest [ ("REQUEST_METHOD", "POST")
                         , ("CONTENT_TYPE", "multipart/form-data; boundary=----------------------------6d9817ad0e6b")
                         , ("CONTENT_LENGTH", show $ BS.length pr2_content)
                         ] pr2_content utf8Encoding

-- application/x-www-form-urlencoded
test_getPOST_Missing_urlenc =
    Nothing ~=? getPOST "test" postRequest1
test_getPOST_urlenc  =
    Just "bar2" ~=? getPOST "foo" postRequest1
test_getPOSTlist_urlenc =
    ["bar", "bar2"] ~=? getPOSTlist "foo" postRequest1

-- multipart/form-data
test_getPOST_Missing_mp =
    Nothing ~=? getPOST "test" postRequest2
test_getPOST_mp  =
    Just "bar2" ~=? getPOST "foo" postRequest2
test_getPOSTlist_mp =
    ["bar", "bar2"] ~=? getPOSTlist "foo" postRequest2


tests = test [
          testMethod
        , testPath
        , testPathMissing
        , testPathUtf8
        , testRequestUriRaw
        , test_getPOST_Missing_urlenc
        , test_getPOST_urlenc
        , test_getPOSTlist_urlenc
        , test_getPOST_Missing_mp
        , test_getPOST_mp
        , test_getPOSTlist_mp
        ]
