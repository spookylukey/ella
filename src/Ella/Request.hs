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
                   , files
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
                    -- * Files
                   , FileInput(..)
                   , ContentType(..)
                   )

where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.List (partition)
import qualified Data.Map as Map
import Data.Maybe
import Network.CGI.Protocol (takeInput, formDecode)
import Network.CGI.Multipart
import Network.CGI.Header
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
      environment :: Map.Map String String -- ^ a map containing the CGI environment var
    , requestBody :: ByteString -- ^ the body of the HTTP request
    , requestEncoding :: Encoding -- ^ the encoding used to interpret the request
    , _env :: [(String, String)]
     -- | All of the POST name-value pairs.  Use this if you need to iterate
     -- through POST variables
    , allPOST :: [(String, String)]
    , _postInputMap :: Map.Map String String
    -- | All of the GET name-value pairs.  Use this if you need to iterate
    -- through GET variables
    , allGET :: [(String, String)]
    , _getInputMap :: Map.Map String String
    , files :: Map.Map String FileInput -- ^ a map of all uploaded files
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
             , allPOST = pvs
             , _postInputMap = Map.fromList pvs -- later vals overwrite earlier, which we want
             , allGET = gvs
             , _getInputMap = Map.fromList gvs
             , files = Map.fromList fvs
             }
      where
        (pvs, fvs) = bodyInput env body enc      -- post vals, file vals
        gvs = queryInput env enc                 -- get vals


-- | Change a Request's encoding
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
-- This is needed if values are submitted with the same name e.g. for
-- handling HTML SELECT elements
getPOSTlist :: String -> Request -> [String]
getPOSTlist name req = getMatching name (allPOST req)

-- | Retrieve a single query string value (last one wins if there are multiple)
getGET :: (Monad m) => String -> Request -> m String
getGET name req = Map.lookup name $ _getInputMap req

-- | Retrieve all the query string values with the given name
--
-- This is needed if values are submitted with the same name
getGETlist :: String -> Request -> [String]
getGETlist name req = getMatching name (allGET req)


-- Much of the following is taken mainly from CGI.Protocol, with
-- large modifications to add support for encoding and to
-- replace Input with String/FileInput

-- | Decodes the input in the query string
queryInput :: [(String,String)] -- ^ CGI environment variables.
           -> Encoding
           -> [(String,String)] -- ^ Input variables and values.
queryInput env enc = formInputEnc (lookupOrNil "QUERY_STRING" env) enc


-- | Decodes application\/x-www-form-urlencoded inputs.
formInputEnc :: String
             -> Encoding          -- ^ Encoding to use to interpret percent-encoded sequences
             -> [(String,String)] -- ^ Input variables and values.
formInputEnc qs encoding = [(repack n encoding, repack v encoding) | (n,v) <- formDecode qs]

-- | Represents an uploaded file
data FileInput = FileInput { fileFilename :: String -- ^ user supplied filename for file
                           , fileContents :: ByteString -- ^ raw contents of the file
                           , fileContentType :: ContentType -- ^ user supplied content-type of the file
                           } deriving (Read, Show, Eq)

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?


bodyInput :: [(String,String)]
          -> ByteString
          -> Encoding
          -> ([(String,String)], [(String,FileInput)])
bodyInput env inp enc =
   case lookup "REQUEST_METHOD" env of
      Just "POST" ->
          let ctype = lookup "CONTENT_TYPE" env >>= parseContentType
          in decodeBody ctype (takeInput env inp) enc
      _ -> ([], [])

-- | Decodes a POST body
decodeBody :: Maybe ContentType
           -> ByteString
           -> Encoding
           -> ([(String,String)], [(String,FileInput)])
decodeBody ctype inp enc =
    case ctype of
               Just (ContentType "application" "x-www-form-urlencoded" _)
                   -> (formInputEnc (BS.unpack inp) enc, [])
               Just (ContentType "multipart" "form-data" ps)
                   -> multipartDecode ps inp enc
               Just _ -> ([], []) -- unknown content-type, the user will have to
                                  -- deal with it by looking at the raw content
               -- No content-type given, assume x-www-form-urlencoded
               Nothing -> (formInputEnc (BS.unpack inp) enc, [])


-- | Decodes multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> ByteString        -- ^ Request body
                -> Encoding          -- ^ Encoding to use for interpreting
                -> ([(String,String)]
                   ,[(String,FileInput)])  -- ^ Input variables and values, and file inputs
multipartDecode ps inp enc =
    case lookup "boundary" ps of
         Just b -> let MultiPart bs = parseMultipartBody b inp
                    in splitLeftRight $ map (bodyPartToInput enc) bs
         Nothing -> ([],[]) -- FIXME: report that there was no boundary

-- Uses Either to return two different types of value
bodyPartToInput :: Encoding -> BodyPart -> Either (String,String) (String,FileInput)
bodyPartToInput enc (BodyPart hs b) =
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) ->
                  let name = repack (lookupOrNil "name" ps) enc
                      filename = lookup "filename" ps
                  in case filename of
                       Just f -> Right (name, FileInput { fileFilename = repack f enc
                                                        , fileContentType = ctype
                                                        , fileContents = b
                                                        })
                       Nothing -> Left (name, (decoder enc) b)
              _ -> error "No Content-Disposition in input"
    where ctype = fromMaybe defaultInputType (getContentType hs)


--
-- * Utilities
--

-- | Same as 'lookup' specialized to strings, but
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

-- | Partitions a list of [Either a b] into two lists
splitLeftRight xs = let (ls, rs) = partition isLeft xs
                    in (map (either id undefined) ls,
                        map (either undefined id) rs)
isLeft (Left x) = True
isLeft _        = False

getMatching name assoclist = map snd $ filter ((==name) . fst) assoclist
