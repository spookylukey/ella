{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Ella.Framework (
                      -- * Dispatching
                      -- $dispatching
                      dispatchCGI
                     , sendResponseCGI
                     , dispatchRequest
                     , DispatchOptions(..)
                      -- * Defaults
                     , defaultDispatchOptions
                     , defaultRequestOptions
                     , default404
                     , default500
                     -- * Routing mechanism
                     , View
                     -- $routing
                     , route
                     , (//->)
                     -- * Matchers
                     , PartMatch
                     -- $matchers
                     , fixedString
                     , intParam
                     , stringParam
                     , anyPath
                     , empty
                     , (</>)
                     , (</+>)
                     , (<+/>)
                     )

where

import Control.Monad ((>=>))
import Data.List (isPrefixOf)
import Ella.GenUtils (apply, utf8, exactParse)
import Ella.Response
import Ella.Request
import System.IO (stdout, hClose)
import qualified Data.ByteString.Lazy.Char8 as BS

-- * Dispatching

-- $dispatching
--
-- The main entry point for handling CGI requests is 'dispatchCGI'.
-- This creates a Request object according to the CGI protocol,
-- dispatches it to a list of views, returning a 404 if no view
-- matches.  This process can be customised using 'DispatchOptions'.
-- A set of defaults for this is provided, 'defaultDispatchOptions',
-- which can be used as a starting point and customised as needed.
--
-- 'dispatchCGI' does not do any error handling.  Since the type of
-- any error handling function will depend on the libraries being
-- used, it is easier to wrap the call to 'dispatchCGI' in your own
-- error handling.  For finer grained error handling, view decorator
-- functions can be used, as well as error handling within the view
-- function itself.

-- | Options for the dispatch process
data DispatchOptions = DispatchOptions {
      notFoundHandler :: Request -> IO Response
    -- ^ function that will return a 404 page in the case of no view functions matching
    , requestOptions :: RequestOptions
    -- ^ options passed to 'buildCGIRequest'
}

type View = Request -> IO (Maybe Response)

-- * Defaults

-- | A basic 404 response that is used by 'defaultDispatchOptions'
default404 :: Response
default404 = buildResponse [
              setStatus 404,
              addContent "<h1>404 Not Found</h1>\n<p>Sorry, the page you requested could not be found.</p>"
             ] utf8HtmlResponse

-- | A basic 500 response, not used internally.
default500 :: String -> Response
default500 content = buildResponse [ setStatus 500
                                   , addContent "<h1>500 Internal Server Error</h1>\n"
                                   , addContent $ utf8 content
                                   ] utf8HtmlResponse

-- | Default options used for interpreting the request
defaultRequestOptions :: RequestOptions
defaultRequestOptions = RequestOptions {
                          encoding = utf8Encoding
                        }

-- | A set of DispatchOptions useful as a basis.
defaultDispatchOptions :: DispatchOptions
defaultDispatchOptions = DispatchOptions {
                           notFoundHandler = const $ return $ default404
                         , requestOptions = defaultRequestOptions
                         }

-- | Used by dispatchCGI, might be useful on its own, especially in testing
--
-- Effectively this reduces a list of view functions so that
-- they act as a single one
dispatchRequest :: [View] -> View
dispatchRequest [] req = return Nothing
dispatchRequest (v:vs) req = do
  resp <- v req
  case resp of
    Nothing -> dispatchRequest vs req
    x -> return x

-- | Sends a Response according to the CGI protocol
sendResponseCGI :: Response -> IO ()
sendResponseCGI resp = do
  BS.hPut stdout (formatResponse resp)
  hClose stdout

-- | Handle a CGI request using a list of possible views
-- If a view returns 'Nothing' the next will be tried,
-- and a 404 issued if all return nothing
dispatchCGI :: [View]           -- ^ list of views functions that will be tried in order
            -> DispatchOptions  -- ^ options to use in dispatching
            -> IO ()
dispatchCGI views opts = do
  req <- buildCGIRequest (requestOptions opts)
  resp' <- dispatchRequest views req
  resp <- case resp' of
            Nothing -> notFoundHandler opts $ req
            Just x -> return x
  sendResponseCGI resp


-- Routing

-- $routing
--
-- The routing mechanism has been designed so that you can write code like the following:
--
-- > routes = [
-- >            empty                                  //-> indexView                 $ decs
-- >          , "posts/" <+/> empty                    //-> postsView                 $ []
-- >          , intParam                               //-> viewWithIntParam          $ []
-- >          , stringParam                            //-> viewWithStringParam       $ []
-- >          , intParam </+> "test/"                  //-> viewWithIntParam          $ []
-- >          , "test/" <+/> intParam                  //-> viewWithIntParam          $ []
-- >          , intParam </> stringParam               //-> viewWithIntAndStringParam $ []
-- >          , intParam </> stringParam </> intParam  //-> viewWithIntStringInt      $ []
-- >          ]
--
-- where:
--
-- >  postsView, indexView :: Request -> IO (Maybe Response)
-- >  viewWithStringParam :: String -> Request -> IO (Maybe Response)
-- >  viewWithIntParam :: Int -> Request -> IO (Maybe Response)
-- >  viewWithIntAndStringParam :: Int -> String -> Request -> IO (Maybe Response)
-- >  viewWithIntStringInt :: Int -> String -> Int -> Request -> IO (Maybe Response)
-- >  decs :: [View -> View]
--
-- These would correspond to URLs like the following:
--
-- > /
-- > /posts/
-- > /123/             123 captured
-- > /abc7/            "abc7" captured
-- > /123/test/        123 captured
-- > /test/123/        123 captured
-- > /123/abc7/        123 and "abc7" captured
-- > /123/abc7/456/    123, "abc7" and 456 captured
--
-- The right hand argument of '//->' is a 'view like' function, of type
-- 'View' OR @a -> 'View'@ OR @a -> b -> 'View'@ etc,
--
-- The left hand argument of '//->' is a \'matcher\' - it parses the
-- path of the Request, optionally capturing parameters and returning
-- a function that will adapt the right hand argument so that it has
-- type View.
--
-- Matchers can be composed using '</>'.  To match a fixed string
-- without capturing, use @fixedString "thestring"@. The operators
-- '</+>' amd '<+/>' are useful for combining fixed strings with other
-- matchers.  To match just a fixed string, you can use
--
-- > "thestring/" <+/> empty
--
-- instead of:
--
-- > fixedString "thestring/"
--
-- The result of the '//->' operator needs to be applied to a list of \'view
-- decorator\' functions, (which may be an empty list) e.g. \'decs\'
-- above.  These decorators take a View and return a View, or
-- alternatively they take a View and a Request and return an IO
-- (Maybe Response).  These means they can be used to do
-- pre-processing of the request, and post-processing of the response.
--
-- The routing mechanism is extensible -- just define your own matchers.
--
-- NB. The Request object trims any leading slash on the path to normalise
-- it, and also to simplify this parsing stage, so do not attempt to match
-- an initial leading slash.


-- | type alias used to simplify signatures
type PartMatch a = (String, a, Request)

-- $matchers
--
-- Matching functions take a 'PartMatch' and return a @Maybe 'PartMatch'@.
-- The first component of 'PartMatch' is a String which is the
-- remaining part of the 'Ella.Request.pathInfo' still to be matched.
--
-- The second component of 'PartMatch' is a 'View' function, or a
-- function that returns a View when partially applied.  This allows for
-- matchers that also capture parameters (of different types) and feed
-- them to the view functions.  In this case, the 'PartMatch' output
-- will have a different type to the 'PartMatch' input.
--
-- The third component of 'PartMatch' is the entire 'Request' object.
-- This allows matchers to operate on other attributes of the 'Request'
-- e.g. only match GET requests.  It also allows them to alter
-- the Request object that a view function receives.

-- | Match a string at the beginning of the path
fixedString :: String -> PartMatch a -> Maybe (PartMatch a)
fixedString s (path, f, r) = if s `isPrefixOf` path
                             then Just (drop (length s) path, f, r)
                             else Nothing

-- | Convenience no-op matcher, useful for when you only want to match
-- a fixed string, or to match an empty string.
empty :: PartMatch a -> Maybe (PartMatch a)
empty = Just


-- | matcher that matches any remaining path
anyPath :: PartMatch a -> Maybe (PartMatch a)
anyPath (path, f, r) = Just ("", f, r)

nextChunk path = let (start, end) = break (== '/') path
                 in case end of
                      [] -> Nothing
                      x:rest -> Just (start, rest)

-- | Matcher that captures a string component followed by a forward slash
stringParam :: PartMatch (String -> a) -> Maybe (PartMatch a)
stringParam (path, f, r) = do
  (chunk, rest) <- nextChunk path
  Just (rest, f chunk, r)

-- | Matcher that captures an integer component followed by a forward slash
intParam :: PartMatch (Int -> a) -> Maybe (PartMatch a)
intParam (path, f, r) = do
  (chunk, rest) <- nextChunk path
  val <- exactParse chunk
  Just (rest, f val, r)

-- | Combine two matchers
(</>) :: (PartMatch a -> Maybe (PartMatch b)) -- ^ LH matcher
      -> (PartMatch b -> Maybe (PartMatch c)) -- ^ RH matcher
      -> (PartMatch a -> Maybe (PartMatch c))
(</>) = (>=>) -- It turns out that this does the job!

-- | Convenience operator for combining a fixed string after a matcher
(</+>) :: (PartMatch a -> Maybe (PartMatch b))  -- ^ matcher
       -> String                              -- ^ fixed string
       -> (PartMatch a -> Maybe (PartMatch b))
matcher </+> str = matcher </> (fixedString str)

-- | Convenience operator for combining a matcher after a fixed string
(<+/>) :: String                              -- ^ fixed string
       -> (PartMatch a -> Maybe (PartMatch b))  -- ^ matcher
       -> (PartMatch a -> Maybe (PartMatch b))
str <+/> matcher = (fixedString str) </> matcher

-- | Apply a matcher to a View (or View-like function that takes
-- additional parameters) to get a View that only responds to the
-- matched URLs
route :: (PartMatch a -> Maybe (PartMatch View)) -- ^ matcher
      -> a                                       -- ^ view-like function
      -> [View -> View]                          -- ^ optional view decorators (processors)
      -> View
route matcher f decs =
    \req -> let match = matcher (pathInfo req, f, req)
            in case match of
                 Nothing -> return Nothing
                 Just (remainder, view, req') -> if null remainder
                                                 then (apply decs view) req'
                                                 else return Nothing

-- | Alias for 'route', see above examples.
(//->) :: (PartMatch a -> Maybe (PartMatch View))
          -> a
          -> [View -> View]
          -> Request
          -> IO (Maybe Response)
(//->) = route
