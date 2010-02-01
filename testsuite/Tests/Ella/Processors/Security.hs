{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Processors.Security

where

import Data.Digest.Pure.SHA (showDigest, sha1)
import Ella.GenUtils (utf8, with, getTimestamp)
import Ella.Processors.Security
import Ella.Request
import Ella.Response
import Ella.TestUtils (mkGetReq, mkPostReq, addCookieValues)
import System.Locale (defaultTimeLocale)
import System.Time (toUTCTime, ClockTime(..), formatCalendarTime, CalendarTime)
import Test.HUnit
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

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
signCookieVal :: String -> Maybe CalendarTime -> String
signCookieVal val expires = (showDigest $ sha1 $ utf8 ("signedcookies:" ++ scp_secret ++ ":" ++ showExpires expires ++ ":" ++ val)) ++ ":" ++ showExpires expires ++ ":" ++ val

showExpires :: Maybe CalendarTime -> String
showExpires (Just x) = formatCalendarTime defaultTimeLocale "%s" x
showExpires Nothing = ""

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
      return ((cookieValue $ head $ cookies resp) == signCookieVal "bar" Nothing)
    ) ~? "signedCookiesProcessor adds sha1 hash to beginning of values"


cookieReq = (mkRequest
             [("REQUEST_METHOD", "GET")
             ,("PATH_INFO", "/posts")
             ,("HTTP_COOKIE",
               "name1=" ++ signCookieVal "val1" Nothing ++
               ";name2=val2;name3=" ++ signCookieVal "val3" Nothing)]
             "" utf8Encoding)

testSignedCookiesProcessor4 =
    (do
      Just resp <- scp viewDisplayingCookies cookieReq
      return (content resp == "name1=val1\nname3=val3\n")
    ) ~? "signedCookiesProcessor removes cookies that don't have correct hashes"

cookieReq2 :: Int -> Request
cookieReq2 ts = (mkRequest
                 [("REQUEST_METHOD", "GET")
                 ,("PATH_INFO", "/posts")
                 ,("HTTP_COOKIE",
                   "name1=" ++ signCookieVal "val1" (mkExpires $ ts + 100) ++
                   ";name2=" ++ signCookieVal "val2" (mkExpires $ ts - 100))]
                 "" utf8Encoding)
    where mkExpires timestamp = Just $ toUTCTime $ TOD (toInteger timestamp) 0


testSignedCookiesProcessorTimestamp =
    (do
      ts <- getTimestamp
      let req = cookieReq2 ts
      Just resp <- scp viewDisplayingCookies req
      return (content resp == "name1=val1\n")
    ) ~? "signedCookiesProcessor removes cookies which have expired"

-- CSRF view processor

csrfTestViewNoToken req = return $ Just $ buildResponse [ addContent "OK"
                                                        ] emptyResponse
csrfTestViewWithToken req = return $ Just $ buildResponse [ addContent $ utf8 $ csrfTokenField csrfProtection $ req
                                                          ] utf8HtmlResponse

csrfRejectionView req = return $ Just $ buildResponse [ addContent "Rejected"
                                                      , setStatus 403
                                                      ] emptyResponse

csrfProtection = mkCSRFProtection (Cookie { cookieName = "csrf"
                                          , cookieValue = undefined
                                          , cookieDomain = Nothing
                                          , cookiePath = Nothing
                                          , cookieExpires = Nothing
                                          , cookieSecure = False })
                 csrfRejectionView "secret"

protectedViewWithToken = (csrfViewProcessor csrfProtection) csrfTestViewWithToken
protectedViewNoToken = (csrfViewProcessor csrfProtection) csrfTestViewNoToken

aCsrfToken = "01234567890123456789"
-- Utility function for adding a valid CSRF cookie to a Request
addCsrfCookie = addCookieValues [("csrf", aCsrfToken)]

testCsrfGETAllowAll =
    (do
      let req = mkGetReq "/foo/"
      Just resp <- protectedViewNoToken req
      return (content resp == "OK")
    ) ~? "csrf protection allows through GET requests"

testCsrfRejectMissingCookie =
    (do
      let req = mkPostReq "/foo/" [("name", "val")
                                  ,("csrftoken", aCsrfToken)]
      Just resp <- protectedViewWithToken req
      return (content resp == "Rejected")
    ) ~? "csrf protection disallows POST requests without CSRF cookie"

testCsrfRejectMissingToken =
    (do
      let req = mkPostReq "/foo/" [("name", "val")] `with` [ addCsrfCookie ]
      Just resp <- protectedViewWithToken req
      return (content resp == "Rejected")
    ) ~? "csrf protection disallows POST requests without CSRF token"

testCsrfRejectMismatchedToken =
    (do
      let req = mkPostReq "/foo/" [("name", "val")
                                  ,("csrftoken", "x")] `with` [ addCsrfCookie ]
      Just resp <- protectedViewWithToken req
      return (content resp == "Rejected")
    ) ~? "csrf protection disallows POST requests when cookie doesn't match token"

testCsrfAcceptMatchingToken =
    (do
      let req = mkPostReq "/foo/" [("name", "val"),
                                   ("csrftoken", aCsrfToken)] `with` [ addCsrfCookie ]
      Just resp <- protectedViewNoToken req
      return (content resp == "OK")
    ) ~? "csrf protection allows POST when cookie matches token"

testCsrfSetsOutgoingCookie =
    (do
      let req = mkGetReq "/foo/"
      Just resp <- protectedViewWithToken req
      case cookies resp of
          [] -> return False
          (c:cs) -> return (cookieName c == "csrf")
    ) ~? "csrf protection sets outgoing cookie"

testCsrfSetsSameOutgoingCookie =
    (do
      let req = mkGetReq "/foo/" `with` [ addCsrfCookie ]
      Just resp <- protectedViewWithToken req
      case cookies resp of
          [] -> return False
          (c:cs) -> if (cookieName c == "csrf")
                      then return (cookieValue c == aCsrfToken)
                      else return False
    ) ~? "csrf protection sets same outgoing cookie as incoming one, if it exists"

-- If the token isn't used in outgoing response, the view processor should not
-- add the cookie.  This is to stop cookies being sent with every request.
testCsrfNoOutgoingCookieIfNoToken =
    (do
      let req = mkGetReq "/foo/"
      Just resp <- protectedViewNoToken req
      return ((length $ cookies resp) == 0)
    ) ~? "csrf protection doesn't send outgoing cookie if token not in response content"

testCsrfSetsTokenInRequestEnv =
    (do
      let req = mkGetReq "/foo/"
          -- view that extracts 'csrftoken' from request environment field
          view = \req -> return $ Just $ buildResponse [ addContent $ utf8 $ Map.findWithDefault "" "csrftoken" $ environment req ] utf8TextResponse
      Just resp <- (csrfViewProcessor csrfProtection) view req
      return ((BS.length $ content resp) > 1)
    ) ~? "csrf processor puts token into request environment"

testCsrfTokenField =
    (do
      let req = mkGetReq "/foo/" `with` [ addCsrfCookie ]
          -- view that extracts 'csrftoken' from request environment field
          view = \req -> return $ Just $ buildResponse [ addContent $ utf8 $ csrfTokenField csrfProtection $ req ] utf8TextResponse
      Just resp <- (csrfViewProcessor csrfProtection) view req
      return (content resp == utf8 ("<div style=\"display:none\"><input type=\"hidden\" name=\"csrftoken\" value=\"" ++ aCsrfToken ++ "\" ></div>"))
    ) ~? "csrf hidden input field is correct"


tests = test [ testSignedCookiesProcessor1
             , testSignedCookiesProcessor2
             , testSignedCookiesProcessor3
             , testSignedCookiesProcessor4
             , testSignedCookiesProcessorTimestamp
             , testCsrfGETAllowAll
             , testCsrfRejectMissingCookie
             , testCsrfRejectMissingToken
             , testCsrfRejectMismatchedToken
             , testCsrfAcceptMatchingToken
             , testCsrfSetsOutgoingCookie
             , testCsrfSetsSameOutgoingCookie
             , testCsrfNoOutgoingCookieIfNoToken
             , testCsrfSetsTokenInRequestEnv
             , testCsrfTokenField
             ]
