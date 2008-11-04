module Ella.Processors.General
    (
     -- * Processors
     -- $processors
     addSlashRedirectView
    )

where

import Data.List (isSuffixOf)
import Ella.Request
import Ella.Response

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
--  @addSlashRedirectView@
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
                  Just x | ("/" `isSuffixOf` x) -> Nothing -- slash is already there
                  Just x  ->  Just $ redirectResponse (x ++ "/")
