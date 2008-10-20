{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Processors.General

where

import Data.Maybe (fromJust)
import Web.Processors.General
import Web.GenUtils ()
import Web.Response
import Web.Request
import Test.HUnit
import Tests.Web.Framework (mkGetReq)

testAddSlashRedirectView1 =
    (do
      resp <- addSlashRedirectView (mkGetReq "/posts")
      return (resp == (Just $ redirectResponse "/posts/"))
    ) ~? "addSlashRedirectView should add a slash if not present at end"


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
             , testAddSlashRedirectView2
             , testAddSlashRedirectView3
             ]
