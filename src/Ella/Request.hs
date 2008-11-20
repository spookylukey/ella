module Ella.Request (
                    -- * Requests
                     Request
                   , RequestOptions(..)
                    -- ** Accessors of Request
                   , requestMethod
                   , pathInfo
                   , requestUriRaw
                   , environment
                   , allPOST
                   , allGET
                   , getPOST
                   , getPOSTlist
                   , getGET
                   , getGETlist
                    -- ** Constructors for Request
                   , mkRequest
                   , buildCGIRequest
                   , changeEncoding
                   -- * Escaping
                   , escapePath
                   , escapePathWithEnc
                    -- * Encodings
                   , Encoding(..)
                   , utf8Encoding
                   )

where

import Network.CGI.Protocol (takeInput, formDecode, Input(..), inputValue, inputFilename, inputContentType)
import Network.CGI.Multipart
import Network.CGI.Header


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
    , allPOST :: [(String, String)] -- ^ all of the POST name-value pairs
    , _postInputMap :: Map.Map String String
    , allGET :: [(String, String)] -- ^ all of the GET name-value pairs
    , _getInputMap :: Map.Map String String
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
             , allPOST = pv
             , _postInputMap = Map.fromList pv -- later vals overwrite earlier, which we want
             , allGET = gv
             , _getInputMap = Map.fromList gv
             }
      where
        pv = map repack_inp $ fst $ bodyInput env body
        gv = queryInput env enc
        -- TODO - rewrite bodyInput, queryInput so that repack_inp is not necessary
        repack_inp (name, val) = (repack name enc, (decoder enc) (inputValue val))

-- | Change a Request encoding
--
-- Reinterprets the data in a Request according to new encoding.  It
-- is not enough to just change the 'encoding' value, as some data
-- that depends on the encoding has already been created.
changeEncoding :: Encoding -> Request -> Request
changeEncoding enc req = mkRequest (_env req) (requestBody req) enc

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
getPOST name req = Map.lookup name $ _postInputMap req

-- | Retrieve all the POST values with the given name
--
-- This is needed if values are submitted with the same name
getPOSTlist :: String -> Request -> [String]
getPOSTlist name req = getMatching name (allPOST req)

-- | Retrieve a single query string value
getGET :: (Monad m) => String -> Request -> m String
getGET name req = Map.lookup name $ _getInputMap req

-- | Retrieve all the query string values with the given name
getGETlist :: String -> Request -> [String]
getGETlist name req = getMatching name (allGET req)

getMatching name assoclist = map snd $ filter ((==name) . fst) assoclist


-- The following is taken mainly from CGI.Protocol, with modifications


queryInput :: [(String,String)] -- ^ CGI environment variables.
           -> Encoding
           -> [(String,String)] -- ^ Input variables and values.
queryInput env enc = formInputEnc (lookupOrNil "QUERY_STRING" env) enc


-- | Decodes application\/x-www-form-urlencoded inputs.
formInputEnc :: String
             -> Encoding          -- ^ Encoding to use to interpret bytes
             -> [(String,String)] -- ^ Input variables and values.
formInputEnc qs encoding = [(repack n encoding, repack v encoding) | (n,v) <- formDecode qs]


-- | Decodes application\/x-www-form-urlencoded inputs.
formInput :: String
          -> [(String,Input)] -- ^ Input variables and values.
formInput qs = [(n, simpleInput v) | (n,v) <- formDecode qs]


-- | Builds an 'Input' object for a simple value.
simpleInput :: String -> Input
simpleInput v = Input { inputValue = BS.pack v,
                        inputFilename = Nothing,
                        inputContentType = defaultInputType }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?


-- TODO - rewrite bodyInput, queryInput etc. so that they use a supplied encoding

bodyInput :: [(String,String)]
          -> ByteString
          -> ([(String,Input)], ByteString)
bodyInput env inp =
   case lookup "REQUEST_METHOD" env of
      Just "POST" ->
          let ctype = lookup "CONTENT_TYPE" env >>= parseContentType
           in decodeBody ctype $ takeInput env inp
      _ -> ([], inp)

-- | Decodes a POST body.
decodeBody :: Maybe ContentType
           -> ByteString
           -> ([(String,Input)], ByteString)
decodeBody ctype inp =
    case ctype of
               Just (ContentType "application" "x-www-form-urlencoded" _)
                   -> (formInput (BS.unpack inp), BS.empty)
               Just (ContentType "multipart" "form-data" ps)
                   -> (multipartDecode ps inp, BS.empty)
               Just _ -> ([], inp) -- unknown content-type, the user will have to
                            -- deal with it by looking at the raw content
               -- No content-type given, assume x-www-form-urlencoded
               Nothing -> (formInput (BS.unpack inp), BS.empty)


-- | Decodes multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> ByteString        -- ^ Request body
                -> [(String,Input)]  -- ^ Input variables and values.
multipartDecode ps inp =
    case lookup "boundary" ps of
         Just b -> let MultiPart bs = parseMultipartBody b inp
                    in map bodyPartToInput bs
         Nothing -> [] -- FIXME: report that there was no boundary

bodyPartToInput :: BodyPart -> (String,Input)
bodyPartToInput (BodyPart hs b) =
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) ->
                  (lookupOrNil "name" ps,
                   Input { inputValue = b,
                           inputFilename = lookup "filename" ps,
                           inputContentType = ctype })
              _ -> ("ERROR",simpleInput "ERROR") -- FIXME: report error
    where ctype = fromMaybe defaultInputType (getContentType hs)


--
-- * Utilities
--

-- | Same as 'lookup' specialized to strings, but
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

