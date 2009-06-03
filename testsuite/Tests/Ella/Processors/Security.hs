{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Processors.Security

where

import Data.Digest.Pure.SHA (showDigest, sha1)
import Ella.GenUtils (utf8)
import Ella.Processors.Security
import Ella.Request
import Ella.Response
import Ella.TestUtils (mkGetReq)
import Test.HUnit

scp_secret = "test"
scp = signedCookiesProcessor scp_secret

viewReturningCookie req = return $ Just $ buildResponse [ addCookie Cookie { cookieName = "foo"
                                                                           , cookieValue = "bar"
                                                                           , cookieExpires = Nothing
                                                                           , cookieDomain = Nothing
                                                                           , cookiePath = Nothing
                                                                           , cookieSecure = False
                                                                   }
                                                        ] emptyResponse

viewDisplayingCookies req = return $ Just $ buildResponse [
                             addContent $ utf8 $ concat [name ++ "=" ++ val ++ "\n" | (name, val) <- allCookies req]
                            ] utf8TextResponse

-- This mirrors how signedCookiesProcessors does it.  Duplication
-- is allowed to ensure test actually does the right thing!
signCookieVal val = (showDigest $ sha1 $ utf8 ("signedcookies" ++ scp_secret ++ val)) ++ ":" ++ val

testSignedCookiesProcessor1 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return (1 == (length $ cookies resp))
    ) ~? "signedCookiesProcessor leaves number of cookies the same"

testSignedCookiesProcessor2 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return ((cookieName $ head $ cookies resp) == "foo")
    ) ~? "signedCookiesProcessor leaves names of cookies the same"

testSignedCookiesProcessor3 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return ((cookieValue $ head $ cookies resp) == signCookieVal "bar")
    ) ~? "signedCookiesProcessor adds sha1 hash to beginning of values"


cookieReq = (mkRequest
             [("REQUEST_METHOD", "GET")
             ,("PATH_INFO", "/posts")
             ,("HTTP_COOKIE",
               "name1=" ++ signCookieVal "val1" ++
               ";name2=val2;name3=" ++ signCookieVal "val3")]
             "" utf8Encoding)

testSignedCookiesProcessor4 =
    (do
      Just resp <- scp viewDisplayingCookies cookieReq
      return (content resp == "name1=val1\nname3=val3\n")
    ) ~? "signedCookiesProcessor removes cookies that don't have correct hashes"

tests = test [ testSignedCookiesProcessor1
             , testSignedCookiesProcessor2
             , testSignedCookiesProcessor3
             , testSignedCookiesProcessor4
             ]
