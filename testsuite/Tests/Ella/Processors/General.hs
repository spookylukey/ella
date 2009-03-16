{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Processors.General

where

import Data.Digest.Pure.SHA (showDigest, sha1)
import Data.Maybe (fromJust)
import Ella.GenUtils (utf8)
import Ella.Processors.General
import Ella.Response
import Ella.Request
import Test.HUnit
import Ella.TestUtils (mkGetReq)

testAddSlashRedirectView1 =
    (do
      resp <- addSlashRedirectView (mkGetReq "/posts")
      return (resp == (Just $ redirectResponse "/posts/"))
    ) ~? "addSlashRedirectView should add a slash if not present at end"

testAddSlashRedirectView1_1 =
    (do
      resp <- addSlashRedirectView (mkRequest
                                    [("REQUEST_METHOD", "GET")
                                    ,("PATH_INFO", "/posts")
                                    ,("REQUEST_URI","/posts?p=2")]
                                    "" utf8Encoding)
      return (resp == (Just $ redirectResponse "/posts/?p=2"))
    ) ~? "addSlashRedirectView should add a slash if not present before query string"


testAddSlashRedirectView2 =
    (do
      resp <- addSlashRedirectView (mkGetReq "/posts/")
      return (resp == Nothing)
    ) ~? "addSlashRedirectView should not redirect if slash present at end"

testAddSlashRedirectView3 =
    (do
      resp <- addSlashRedirectView (mkRequest
                                    [("REQUEST_METHOD", "GET")
                                    ,("PATH_INFO", "/posts")
                                    ,("REQUEST_URI","/foo/posts")]
                                    "" utf8Encoding)
      return (resp == (Just $ redirectResponse "/foo/posts/"))
    ) ~? "addSlashRedirectView should redirect based on request URI, not path info"

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

signCookieVal val = (showDigest $ sha1 $ utf8 (scp_secret ++ val)) ++ ":" ++ val

testSignedCookiesProcessor1 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return (1 == (length $ cookies resp))
    ) ~? "signCookiesProcessor leaves number of cookies the same"

testSignedCookiesProcessor2 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return ((cookieName $ head $ cookies resp) == "foo")
    ) ~? "signCookiesProcessor leaves names of cookies the same"

testSignedCookiesProcessor3 =
    (do
      Just resp <- scp viewReturningCookie (mkGetReq "/foo/")
      return ((cookieValue $ head $ cookies resp) == signCookieVal "bar")
    ) ~? "signCookiesProcessor adds sha1 hash to beginning of values"

tests = test [ testAddSlashRedirectView1
             , testAddSlashRedirectView1_1
             , testAddSlashRedirectView2
             , testAddSlashRedirectView3
             , testSignedCookiesProcessor1
             , testSignedCookiesProcessor2
             , testSignedCookiesProcessor3
             ]
