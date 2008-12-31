{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Response

where

import Ella.Response
import Test.HUnit
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (sort)

testAddContent1 = "“Hello”" ~=? (content $ addContent "“Hello”" $ emptyResponse)

testAddContent2 = "Hello world" ~=? (content $ addContent " world" $ addContent "Hello" $ emptyResponse)

testBuildResponse = "hello world" ~=? (content $
                                       buildResponse [ addContent "hello"
                                                     , addContent " world"
                                                     ] utf8HtmlResponse)

testFormatResponse = "Content-type: text/html; charset=UTF-8\r\n\
                     \Status: 200\r\n\
                     \\r\n\
                     \<h1>Test</h1>" ~=?
                     (formatResponse $ buildResponse [
                                          addContent "<h1>Test</h1>"
                                         ] utf8HtmlResponse)

testFormatResponse2 = "Content-type: text/html; charset=UTF-8\r\n\
                      \Status: 404\r\n\
                      \\r\n\
                      \<h1>404 Not Found</h1>" ~=?
                      (formatResponse $ buildResponse [
                                           addContent "<h1>404 Not Found</h1>"
                                          , setStatus 404
                                          ] utf8HtmlResponse)

-- insert
testSetHeader1 = [(HeaderName "Header1", "value 1")] ~=?
                 (headers $ setHeader "Header1" "value 1" emptyResponse)

-- update
testSetHeader2 = [(HeaderName "Header1", "value 1.1"),
                  (HeaderName "Header2", "value 2")] ~=?
                 (sort $ headers $ buildResponse [ setHeader "Header1" "value 1"
                                                 , setHeader "Header2" "value 2"
                                                 , setHeader "Header1" "value 1.1"
                                                 ] emptyResponse)

-- Replacement should be case insensitive
testSetHeader3 = [(HeaderName "Header1", "value 1.1")] ~=?
                 (sort $ headers $ buildResponse [ setHeader "Header1" "value 1"
                                                 , setHeader "header1" "value 1.1"
                                                 ] emptyResponse)

testRedirectResponse = "Location: /foo/bar/\r\n\
                        \Status: 302\r\n\
                        \\r\n" ~=?
                        (formatResponse $ redirectResponse "/foo/bar/")

-- cookies
-- Most of the implementation is from Network.CGI.Cookies,
-- so we don't bother testing thoroughly
testAddCookie = "Set-Cookie: foo=bar\r\n\
                \Status: 200\r\n\
                \\r\n" ~=?
                (formatResponse $ buildResponse [ addCookie Cookie { cookieName = "foo"
                                                                   , cookieValue = "bar"
                                                                   , cookieExpires = Nothing
                                                                   , cookieDomain = Nothing
                                                                   , cookiePath = Nothing
                                                                   , cookieSecure = False
                                                                   }
                                                ] emptyResponse)

tests = test [
          testAddContent1
        , testAddContent2
        , testBuildResponse
        , testFormatResponse
        , testFormatResponse2
        , testSetHeader1
        , testSetHeader2
        , testSetHeader3
        , testRedirectResponse
        , testAddCookie
        ]
