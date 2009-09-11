module Ella.Processors.Security ( signedCookiesProcessor
                                , CSRFProtection(..)
                                , mkCSRFProtection
                                , defaultCSRFRejectView
                                )

where

import Control.Monad (guard)
import Data.ByteString.Search.KnuthMorrisPratt (matchLL)
import Data.Digest.Pure.SHA (showDigest, sha1)
import Data.Maybe (isJust, fromJust, isNothing)
import Ella.Framework
import Ella.GenUtils (utf8, getTimestamp, randomStr, with)
import Ella.Request
import Ella.Response
import System.Time (ClockTime(..), toUTCTime)
import qualified Data.Map as Map

makeShaHash prefix secret val = showDigest $ sha1 $ utf8 $ prefix ++ secret ++ val

-- | Create view processor for implementing signed cookies.
-- Pass a secret string (used for hashing), and apply the resulting function
-- as a view processor.
signedCookiesProcessor :: String -> (View -> View)
signedCookiesProcessor secret view req =
    do
      -- modify the request to strip invalid cookies
      let req2 = removeInvalidCookies req
      -- get the normal response
      resp' <- view req2
      case resp' of
        Nothing -> return Nothing
        -- Now modify outgoing response
        Just resp -> return $ Just $ resp { cookies = map addShaHash $ cookies resp }
    where
      mkHash val = makeShaHash "signedcookies" secret val
      addShaHash cookie = cookie { cookieValue = (mkHash $ cookieValue cookie) ++ ":" ++ cookieValue cookie }
      retrieveCookieVal fullval = let (hash, val') = span (/= ':') fullval
                                      val = drop 1 val' -- for the ':'
                                  in if mkHash val == hash
                                     then Just val
                                     else Nothing
      removeInvalidCookies req = let checked = do
                                       (name, val) <- allCookies req
                                       let newval = retrieveCookieVal val
                                       guard (isJust newval)
                                       return (name, fromJust newval)
                                 in req { allCookies = checked }

-- | CSRF protection
--
-- Provides a view processor function and other utility functions for protecting
-- against CSRF using a cookie.  It sets a cookie to a random value, provides a
-- function for adding tokens based on the value to outgoing forms, making them
-- unique to each user, and requires incoming POST requests to have the same
-- token.
data CSRFProtection = CSRFProtection {
      csrfViewProcessor :: View -> View -- ^ view processor that stops requests
                                  -- without the CSRF token and sets an outgoing
                                  -- cookie.
    , csrfTokenField :: Request -> String -- ^ function that returns a hidden
                                          -- input field to be inserted into
                                          -- forms.
    , csrfTokenName :: String -- ^ name to use for CSRF token in forms
    , csrfTokenValue :: Request -> String -- ^ Returns the raw CSRF token
                                          -- for a given request
}

defaultCSRFRejectView :: View
defaultCSRFRejectView req = return $ Just $ buildResponse [ addContent $ utf8 "<h1>403 Forbidden</h1>"
                                                          , addContent $ utf8 "<p>CSRF protection triggered, request aborted"
                                                          , setStatus 403
                                                          ] utf8HtmlResponse

-- | Creates a CSRFProtection object for the supplied options.
mkCSRFProtection :: Cookie -- ^ cookie used for basis of CSRF cookie, must have at least 'name' set, 'value' and 'expires' will be overwritten
                 -> View -- ^ view to be used for rejects
                 -> String -- ^ secret string used for hashing
                 -> CSRFProtection
mkCSRFProtection baseCookie rejectView secret =
    let tokenName = "csrftoken"
        requestEnvName = "csrftoken"
        makeCsrfToken = randomStr 20
        getTokenFromReq req = fromJust $ Map.lookup requestEnvName $ environment req
        mkTokenField req = "<div style=\"display:none\"><input type=\"hidden\" name=\"" ++ tokenName ++ "\" value=\"" ++ getTokenFromReq req ++ "\" ></div>"
        addTokenToReq req token = req { environment = Map.insert requestEnvName token $ environment req }

        makeCsrfCookie token = do
          timestamp <- getTimestamp
          let expires = Just $ toUTCTime $ TOD (toInteger timestamp + 3600*24*365*5) 0
          return baseCookie { cookieExpires = expires
                            , cookieValue = token
                            }

        pview view = \req -> do
          -- Get existing CSRF cookie
          let incomingCookie = getCookieVal req (cookieName baseCookie)
          let incomingToken = getPOST req tokenName

          -- normal processing - add token to request environment and
          -- add outgoing cookie
          let normalProc = do
                           -- create token if one doesn't exist
                           token <- do
                               case incomingCookie of
                                 Just val -> return val
                                 _        -> makeCsrfToken
                           -- add token to environment in Request object
                           let req2 = addTokenToReq req token
                           resp' <- view req2
                           case resp' of
                             Nothing -> return Nothing
                             Just resp -> do
                                        -- set cookie on all outgoing responses
                                        -- that have used the token.  (Don't
                                        -- want to set cookie with every
                                        -- response...). This is a fairly brute
                                        -- force and probably inefficient
                                        -- solution
                                        if null $ matchLL (utf8 token) (content resp)
                                           then return (Just resp)
                                           else do
                                               cookie <- makeCsrfCookie token
                                               let resp2 =  resp `with` [ addCookie cookie ]
                                               return (Just resp2)

          -- if POST request, reject if no cookie or no POST token or
          -- POST token doesn't match hash of cookie
          if requestMethod req == "POST"
            then if isNothing incomingCookie || (incomingCookie /= incomingToken)
                   then rejectView req
                   else normalProc
            else normalProc

    in CSRFProtection { csrfViewProcessor = pview
                      , csrfTokenField = mkTokenField
                      , csrfTokenName = tokenName
                      , csrfTokenValue = getTokenFromReq
                      }
