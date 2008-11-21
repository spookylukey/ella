{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Request

where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Ella.Request
import Test.HUnit
import Ella.GenUtils (utf8)

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "" utf8Encoding)
testPath = "foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "" utf8Encoding)
testPathMissing = "" ~=? pathInfo (mkRequest [] "" utf8Encoding)
testPathUtf8 = "\233" ~=? pathInfo (mkRequest [("PATH_INFO", "\195\169")] "" utf8Encoding)
testRequestUriRaw = Just "/root/foo/%C3%A9/" ~=? requestUriRaw (mkRequest [("REQUEST_URI","/root/foo/%C3%A9/")
                                                                          ,("PATH_INFO","/foo/\195\169/")] "" utf8Encoding)

-- application/x-www-form-urlencoded
pr1_content = utf8 "foo=bar&baz=%C3%A9&foo=bar2&%C3%A9=z"
postRequest1 = mkRequest [ ("REQUEST_METHOD", "POST")
                         , ("CONTENT_TYPE", "application/x-www-form-urlencoded")
                         , ("CONTENT_LENGTH", show $ BS.length pr1_content)
                         ] pr1_content utf8Encoding

-- multipart/form-data
pr2_content :: ByteString
pr2_content = utf8 "------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"foo\"\r\n\r\nbar\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"baz\"\r\n\r\n\233\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"\233\"\r\n\r\nz\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"foo\"\r\n\r\nbar2\r\n------------------------------6d9817ad0e6b\r\nContent-Disposition: form-data; name=\"afile\"; filename=\"thefilename\233.txt\"\r\n\r\nThe file contents\233\r\n------------------------------6d9817ad0e6b--\r\n"
postRequest2 = mkRequest [ ("REQUEST_METHOD", "POST")
                         , ("CONTENT_TYPE", "multipart/form-data; boundary=----------------------------6d9817ad0e6b")
                         , ("CONTENT_LENGTH", show $ BS.length pr2_content)
                         ] pr2_content utf8Encoding

postRequest1_b = mkRequest [ ("REQUEST_METHOD", "POST")
                           , ("CONTENT_TYPE", "application/x-www-form-urlencoded")
                           , ("QUERY_STRING", "foo=bar3&frobble=gobble")
                           , ("CONTENT_LENGTH", show $ BS.length pr1_content)
                           ] pr1_content utf8Encoding

-- getPOST --

-- application/x-www-form-urlencoded
test_getPOST_Missing_urlenc =
    Nothing ~=? getPOST "test" postRequest1
test_getPOST_urlenc  =
    Just "bar2" ~=? getPOST "foo" postRequest1
test_getPOST_urlenc2 =
    Just "\233" ~=? getPOST "baz" postRequest1
test_getPOST_urlenc3 =
    Just "z" ~=? getPOST "\233" postRequest1
test_getPOSTlist_urlenc =
    ["bar", "bar2"] ~=? getPOSTlist "foo" postRequest1

-- multipart/form-data
test_getPOST_Missing_mp =
    Nothing ~=? getPOST "test" postRequest2
test_getPOST_mp  =
    Just "bar2" ~=? getPOST "foo" postRequest2
test_getPOSTlist_mp =
    ["bar", "bar2"] ~=? getPOSTlist "foo" postRequest2
test_getPOST_mp2 =
    Just "\233" ~=? getPOST "baz" postRequest2
test_getPOST_mp3 =
    Just "z" ~=? getPOST "\233" postRequest2

-- test that GET and POST are not mixed up
test_getPOST_postonly = Nothing ~=? getPOST "frobble" postRequest1_b


-- getGET --
getRequest1 = mkRequest [("REQUEST_METHOD", "GET")] "" utf8Encoding
getRequest2 = mkRequest [("REQUEST_METHOD", "GET")
                        ,("QUERY_STRING", "frobble=v1&foo=bar&baz=%C3%A9&frobble=v2&%C3%A9=z")] "" utf8Encoding

test_getGET_1 = Nothing ~=? getGET "x" getRequest1
test_getGET_2 = Nothing ~=? getGET "x" getRequest2
test_getGET_3 = Just "bar" ~=? getGET "foo" getRequest2
test_getGET_4 = Just "v2" ~=? getGET "frobble" getRequest2
test_getGET_5 = Just "\233" ~=? getGET "baz" getRequest2
test_getGET_6 = Just "z" ~=? getGET "\233" getRequest2
test_getGETlist_1 = ["v1","v2"] ~=? getGETlist "frobble" getRequest2


-- files
test_files_1 = Just FileInput { fileFilename = "thefilename\233.txt"
                              , fileContents = utf8 "The file contents\233"
                              , fileContentType = ContentType "text" "plain" []
                              } ~=? (Map.lookup "afile" $ files postRequest2)

tests = test [
          testMethod
        , testPath
        , testPathMissing
        , testPathUtf8
        , testRequestUriRaw
        , test_getPOST_Missing_urlenc
        , test_getPOST_urlenc
        , test_getPOST_urlenc2
        , test_getPOST_urlenc3
        , test_getPOSTlist_urlenc
        , test_getPOST_Missing_mp
        , test_getPOST_mp
        , test_getPOST_mp2
        , test_getPOST_mp3
        , test_getPOSTlist_mp
        , test_getPOST_postonly
        , test_getGET_1
        , test_getGET_2
        , test_getGET_3
        , test_getGET_4
        , test_getGET_5
        , test_getGET_6
        , test_getGETlist_1
        , test_files_1
        ]
