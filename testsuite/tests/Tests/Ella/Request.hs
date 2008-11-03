{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Request

where

import Ella.Request
import Test.HUnit
import Ella.GenUtils () -- for IsString instance

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "" utf8Encoding)
testPath = "foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "" utf8Encoding)
testPathMissing = "" ~=? pathInfo (mkRequest [] "" utf8Encoding)
testPathUtf8 = "\233" ~=? pathInfo (mkRequest [("PATH_INFO", "\195\169")] "" utf8Encoding)
testRequestUriRaw = Just "/root/foo/%C3%A9/" ~=? requestUriRaw (mkRequest [("REQUEST_URI","/root/foo/%C3%A9/")
                                                                          ,("PATH_INFO","/foo/\195\169/")] "" utf8Encoding)

tests = test [
          testMethod
        , testPath
        , testPathMissing
        , testPathUtf8
        , testRequestUriRaw
        ]
