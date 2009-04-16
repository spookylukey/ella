module Ella.Processors.Security ( signedCookiesProcessor
                                )

where

import Ella.GenUtils (utf8)
import Data.Digest.Pure.SHA (showDigest, sha1)
import Data.Maybe (isJust, fromJust)
import Ella.Framework
import Control.Monad (guard)
import Ella.Request
import Ella.Response

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
