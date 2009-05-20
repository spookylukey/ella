{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Processors.General

where

import Data.Maybe (fromJust)
import Ella.GenUtils (utf8)
import Ella.Processors.General
import Ella.Request
import Ella.Response
import Ella.TestUtils (mkGetReq)
import Test.HUnit

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


tests = test [ testAddSlashRedirectView1
             , testAddSlashRedirectView1_1
             , testAddSlashRedirectView2
             , testAddSlashRedirectView3
             ]
