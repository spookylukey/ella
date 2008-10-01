{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Web.Framework (
                      -- * Dispatching
                      dispatchCGI
                     , dispatchRequest
                     , DispatchOptions(..)
                     , defaultDispatchOptions
                     , default404
                     , View
                     -- * Routing mechanism
                     -- $routing
                     , routeTo
                     , (//->)
                     -- * Matchers
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
import Web.Response
import Web.Request
import Web.Utils
import System.IO (stdout, hClose)
import qualified Data.ByteString.Lazy.Char8 as BS

-- * Dispatching

data DispatchOptions = DispatchOptions {
      notFoundHandler :: Request -> IO Response -- function that will return a 404 page in the case
                                                -- of no view functions matching
}

type View = Request -> IO (Maybe Response)

default404 = buildResponse [
              setStatus 404,
              addContent "<h1>404 Not Found</h1>\n<p>Sorry, the page you requested could not be found.</p>"
             ] utf8HtmlResponse

defaultDispatchOptions = DispatchOptions {
                           notFoundHandler = const $ return $ default404
                         }

dispatchRequest :: Request -> [View] -> IO (Maybe Response)
dispatchRequest req [] = return Nothing
dispatchRequest req (v:vs) = do
  resp <- v req
  case resp of
    Nothing -> dispatchRequest req vs
    x -> return x

-- | Handle a CGI request using a list of possible views
-- If a view returns 'Nothing' the next will be tried,
-- and a 404 issued if all return nothing
dispatchCGI :: [View] -- list of views functions that will be tried in order
            -> DispatchOptions                  -- options to use in dispatching
            -> IO ()
dispatchCGI views opts = do
  req <- buildCGIRequest
  resp' <- dispatchRequest req views
  resp <- case resp' of
            Nothing -> notFoundHandler opts $ req
            Just x -> return x
  BS.hPut stdout (formatResponse resp)

-- $routing
--
-- The routing mechanism has been designed so that you can write code like the following:
--
-- > routes = [
-- >            empty                                  //-> indexView
-- >          , "posts/" <+/> empty                    //-> postsView
-- >          , intParam                               //-> viewWithIntParam
-- >          , stringParam                            //-> viewWithStringParam
-- >          , intParam </+> "test/"                  //-> viewWithIntParam
-- >          , "test/" <+/> intParam                  //-> viewWithIntParam
-- >          , intParam </> stringParam               //-> viewWithIntAndStringParam
-- >          , intParam </> stringParam </> intParam  //-> viewWithIntStringInt
-- >          ]
--
-- where:
--
-- >  postsView, indexView :: Request -> IO (Maybe Response)
-- >  viewWithStringParam :: String -> Request -> IO (Maybe Response)
-- >  viewWithIntParam :: Int -> Request -> IO (Maybe Response)
-- >  viewWithIntAndStringParam :: Int -> String -> Request -> IO (Maybe Response)
-- >  viewWithIntStringInt :: Int -> String -> Int -> Request -> IO (Maybe Response)
--
-- The right hand argument of //-> is a 'view like' function, of type
-- View OR a -> View OR a -> b -> View etc,
-- where View = Request -> IO (Maybe Response)

-- The left hand argument of '//->' is a \'matcher\' - it parses the
-- 'path' of the Request, optionally capturing parameters and
-- returning a function that will adapt the right hand argument so
-- that it has type View.
--
-- Matchers can be composed using '</>'.  To match a fixed string
-- without capturing, use @fixedString "thestring"@. The operators
-- </+> amd <+/> are useful to combining fixed strings with other
-- matchers.  To match just a fixed string, you can use
--
-- > "thestring/" <+/> empty
--
-- instead of:
--
-- > fixedString "thestring/"
--
-- The routing mechanism is extensible -- just define your own matchers.
--
-- NB. The Request object trims any leading slash on the path to normalise
-- it, and also to simplify this parsing stage, so do not attempt to match
-- an initial leading slash.

-- | Match a string at the beginning of the path
fixedString :: String -> (String, a) -> Maybe (String, a)
fixedString s (path, f) = if s `isPrefixOf` path
                          then Just (drop (length s) path, f)
                          else Nothing

-- | Convenience no-op matcher, useful for when you only want to match
-- a fixed string, or to match an empty string.
empty :: (String, a) -> Maybe (String, a)
empty = Just


-- | matcher that matches any remaining path
anyPath (path, f) = Just ("", f)

nextChunk path = let (start, end) = break (== '/') path
                 in case end of
                      [] -> Nothing
                      x:rest -> Just (start, rest)

-- | Matcher that captures a string component followed by a forward slash
stringParam :: (String, String -> a) -> Maybe (String, a)
stringParam (path, f) = do
  (chunk, rest) <- nextChunk path
  Just (rest, f chunk)

-- | Matcher that captures an integer component followed by a forward slash
intParam :: (String, Int -> a) -> Maybe (String, a)
intParam (path, f) = do
  (chunk, rest) <- nextChunk path
  let parses = reads chunk :: [(Int, String)]
  case parses of
    [(val, "")] -> Just (rest, f val)
    otherwise -> Nothing

-- | Combine two matchers
(</>) :: ((String, a) -> Maybe (String, b)) -- ^ LH matcher
      -> ((String, b) -> Maybe (String, c)) -- ^ RH matcher
      -> ((String, a) -> Maybe (String, c))
(</>) = (>=>) -- It turns out that this does the job!
infixl 3 </>

-- | Convenience operator for combining a fixed string after a matcher
matcher </+> str = matcher </> (fixedString str)
-- | Convenience operator for combining a matcher after a fixed string
str <+/> matcher = (fixedString str) </> matcher

-- | Apply a matcher to a View (or View-like function that takes
-- additional parameters) to get a View that only responds to the
-- matched URLs
routeTo :: ((String, a) -> Maybe (String, View))
        -> a
        -> View
routeTo matcher f = \req -> let match = matcher (pathInfo req, f)
                            in case match of
                                 Nothing -> return Nothing
                                 Just (remainder, view) -> if null remainder
                                                           then view req
                                                           else return Nothing
-- | Alias for 'routeTo'
(//->) = routeTo
infix 2 //->