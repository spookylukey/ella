module Ella.Processors.General
    (
     -- * Processors
     -- $processors
     addSlashRedirectView
    , signedCookiesProcessor
    )

where

import Control.Monad (guard)
import Data.Digest.Pure.SHA (showDigest, sha1)
import Data.List (isSuffixOf)
import Data.Maybe (isJust, fromJust)
import Ella.GenUtils (utf8)
import Ella.Request
import Ella.Response
import Ella.Framework

-- $processors
--
-- Processors are used for doing various tasks, usually not limited
-- in usefulness to one particular view function.
--
-- * View processors
--
--  These take a view function and return a view function.
--  Alternatively, take a view function and a request and return an IO
--  (Maybe Response).  This allows them to do both request
--  pre-processing and response post-processing.  They will be usually
--  be used as \'decorators\' when defining routes.
--
-- * View functions
--
--  These are straightforward view functions which happen to work as a
--  kind of pre-handler.  They are installed using routes, usually
--  before all the others.  These usually do redirects, for example
--  'addSlashRedirectView'
--
-- * Response processors
--
--  These take a response and the original request object, and return
--  a possibly modified response.  This can be useful for
--  post-processing, or adding headers etc.


-- | Returns a responseRedirect if the the request URI does not end
-- with a slash.  Should be installed before all other routes.

-- TODO
-- need to include query string, and think about how to handle
-- POSTs etc
addSlashRedirectView :: Request -> IO (Maybe Response)
addSlashRedirectView req =
    let uri = requestUriRaw req
    in return $ case uri of
                  Nothing ->  Nothing -- Can't do a redirect if we don't know original URI
                  Just "" ->  Nothing -- Don't redirect if empty
                  Just x  ->
                      let (path, qs) = span (/= '?') x
                      in if ("/" `isSuffixOf` path)
                         then Nothing -- slash is already there
                         else Just $ redirectResponse (path ++ "/" ++ qs)

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
      mkHash val = showDigest $ sha1 $ utf8 $ secret ++ val
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
