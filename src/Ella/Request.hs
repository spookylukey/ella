module Ella.Request (
                    -- * Requests
                     Request
                   , RequestOptions(..)
                    -- ** Accessors of Request
                   , requestMethod
                   , pathInfo
                   , requestUriRaw
                   , environment
                   , postInputs
                   , getPOST
                   , getPOSTlist
                    -- ** Constructors for Request
                   , mkRequest, buildCGIRequest
                   -- * Escaping
                   , escapePath
                   , escapePathWithEnc
                    -- * Encodings
                   , Encoding(..)
                   , utf8Encoding
                   )

where

import Network.CGI.Protocol (takeInput, decodeInput, formDecode, Input, inputValue)
--import Network.CGI.Multipart

import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe
import Network.URI (escapeURIString, isUnescapedInURI)
import System.Environment (getEnvironment)
import System.IO (stdin)

-- Encodings

-- | Used to store conversion functions need to interpret CGI requests
data Encoding = Encoding {
      name :: String
    -- ^ descriptive name of the encoding
    , decoder :: ByteString -> String
    -- ^ convert ByteString to unicode string
    , encoder :: String -> ByteString
    -- ^ convert unicode string to ByteString
    }

instance Eq Encoding where
    x == y = name x == name y

instance Show Encoding where
    show x = "Encoding " ++ name x

-- * Defaults

-- | An 'Encoding' for UTF8
utf8Encoding :: Encoding
utf8Encoding = Encoding {
                 name = "UTF8"
               , decoder = UTF8.toString
               , encoder = UTF8.fromString
               }

-- | Options that affect the way that HTTP requests are handled
data RequestOptions = RequestOptions {
      encoding :: Encoding -- ^ Handles request encoding translation
    } deriving (Eq, Show)

-- | Represents a CGI request.  This contains the fundamental data
-- that is passed around the CGI application.  Accessor functions
-- are provided to extract all the useful information.
data Request = Request {
      environment :: Map.Map String String
    , requestBody :: ByteString
    , requestEncoding :: Encoding
    , _env :: [(String, String)]
    , postInputs :: [(String, String)]
    , _postInputMap :: Map.Map String String
    } deriving (Show, Eq)

-- | Create a Request object
mkRequest :: [(String, String)] -- ^ association list of environment variables
          -> ByteString -- ^ lazy ByteString containing request body
          -> Encoding -- ^ Encoding to use for request
          -> Request
mkRequest env body enc
    = let envMap = Map.fromList env
      in Request {
               environment = envMap
             , requestBody = body
             , requestEncoding = enc
             , _env = env
             , postInputs = pv
             , _postInputMap = Map.fromList pv -- later vals overwrite earlier, which we want
             }
      where
        -- This is incorrect (decodeInput includes GET params), but OK for now.
        pv = map repack_inp $ fst $ decodeInput env body
        repack_inp (name, val) = (repack name enc, (decoder enc) (inputValue val))

-- | Returns the request method (GET, POST etc) of the request
requestMethod :: Request -> String
requestMethod request = fromJust $ Map.lookup "REQUEST_METHOD" $ environment request

-- | Returns the path info of the request, with any leading forward slash removed,
-- and percent encoded chars interpreted according to the encoding.
pathInfo :: Request -> String
pathInfo request = let pi = Map.lookup "PATH_INFO" $ environment request
                       -- Normalise to having no leading slash
                       adjusted = case pi of
                                    Nothing -> ""
                                    Just ('/':rest) -> rest
                                    Just path -> path
                   in repack adjusted (requestEncoding request)

-- | Repacks bytes in a string according to an encoding
--
-- PATH_INFO and other vars contains Haskell strings, but they contain
-- uninterpreted byte sequences instead of genuine Unicode chars.  We
-- re-pack as bytes (BS.pack discards anything > \255), and then
-- re-interpret.
repack :: String -> Encoding -> String
repack str encoding = let bytes = BS.pack str
                      in (decoder encoding) bytes

-- | Returns the URI requested by the client, with percent encoding intact
--
-- This can fail if the environment does not pass "REQUEST_URI".  Apache
-- always does pass this, so normally just use 'fromJust' on the answer.
requestUriRaw :: (Monad m) => Request -> m String
requestUriRaw request = Map.lookup "REQUEST_URI" $ environment request


-- | Creates a Request object according to the CGI protocol
buildCGIRequest :: RequestOptions -- ^ options which determine how the HTTP request is interpreted
                -> IO Request
buildCGIRequest opts = do
  env <- getEnvironment
  body <- BS.hGetContents stdin
  return $ mkRequest env body (encoding opts)


-- | Escapes a string of bytes with percent encoding
escapePath :: ByteString -> String
-- Borrowed from Network.URI
escapePath bs = escapeURIString isUnescapedInURIPath $ BS.unpack bs
  where isUnescapedInURIPath c = isUnescapedInURI c && c `notElem` "?#"

-- | Escapes a unicode string with percent encoding, using the supplied
-- bytestring/string Encoder
escapePathWithEnc :: String -> Encoding -> String
escapePathWithEnc s enc = escapePath (encoder enc $ s)


-- | Retrieve a single POST value
getPOST :: (Monad m) => String -> Request -> m String
getPOST name req = do
  let vals = getMatching name (reverse $ postInputs req)
  case vals of
    [] -> fail "POST var not found"
    (x:xs) -> return x

getPOSTlist :: String -> Request -> [String]
getPOSTlist name req = getMatching name (postInputs req)

getMatching name assoclist = map snd $ filter ((==name) . fst) assoclist
