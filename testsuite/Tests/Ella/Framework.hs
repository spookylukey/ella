{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Ella.Framework

where

import qualified Data.ByteString.Lazy.Char8 as BS
import Test.HUnit
import Ella.Framework
import Ella.Request
import Ella.Response
import Data.Maybe (isNothing, isJust)
import Control.Monad (liftM, (>=>))
import Ella.GenUtils (utf8)

req1 = mkGetReq "/posts/"
resp1 = buildResponse [ addContent "resp1" ] utf8HtmlResponse
resp2 = buildResponse [ addContent "resp2" ] utf8HtmlResponse

mkGetReq path = mkRequest [("REQUEST_METHOD","GET")
                          ,("PATH_INFO", path)
                          ,("REQUEST_URI", escapePathWithEnc path utf8Encoding)
                          ] "" utf8Encoding

alwaysFailView = const (return Nothing)
alwaysSucceedView1 = const (return $ Just resp1)
alwaysSucceedView2 = const (return $ Just resp2)

viewWithStringParam1 :: String -> Request -> IO (Maybe Response)
viewWithStringParam1 p req = return $ Just $ viewWithStringParam1' p
viewWithStringParam1' p = buildResponse [
                           addContent $ utf8 ("Got: " ++ p)
                          ] utf8HtmlResponse

viewWithIntParam1 :: Int -> Request -> IO (Maybe Response)
viewWithIntParam1 p req = return $ Just $ viewWithIntParam1' p
viewWithIntParam1' p = buildResponse [
                        addContent $ utf8 ("Got integer: " ++ show p)
                       ] utf8HtmlResponse

viewWithIntParam2 :: Int -> Request -> IO (Maybe Response)
viewWithIntParam2 p req = return $ Just $ viewWithIntParam2' p
viewWithIntParam2' p = buildResponse [
                        addContent $ utf8 ("2: Got integer: " ++ show p)
                       ] utf8HtmlResponse

viewWithIntAndStringParam1 :: Int -> String -> Request -> IO (Maybe Response)
viewWithIntAndStringParam1 i s req = return $ Just $ viewWithIntAndStringParam1' i s
viewWithIntAndStringParam1' i s = buildResponse [
                                   addContent $ utf8 ("Got integer: " ++ show i ++
                                                      " and string: " ++ s)
                                  ] utf8HtmlResponse

viewWithIntStringInt1 :: Int -> String -> Int -> Request -> IO (Maybe Response)
viewWithIntStringInt1 i s i2 req = return $ Just $ viewWithIntStringInt1' i s i2
viewWithIntStringInt1' i s i2 = buildResponse [
                                 addContent $ utf8 ("Got integer 1: " ++ show i ++
                                                    " and string: " ++ s ++
                                                    " and integer 2: "++ show i2)
                                ] utf8HtmlResponse

-- Some of the syntax below is complicated by the fact that the
-- functions being tested all use the IO monad in their type
-- signatures (tho' in these tests they don't actually need any IO),
-- and you cannot pattern match against IO actions.

testDispatchRequest1 = (dispatchRequest [] req1
                        >>= return . isNothing)
                       ~? "With no views, nothing is dispatched"

testDispatchRequest2 = (dispatchRequest  [alwaysFailView] req1
                        >>= return . isNothing)
                       ~? "Should get Nothing if all view return Nothing"

testDispatchRequest3 = (do
                         resp <- dispatchRequest [alwaysFailView,
                                                  alwaysSucceedView1,
                                                  alwaysSucceedView2] req1
                         return $ (resp == (Just resp1) && resp /= (Just resp2)))
                       ~? "Dispatch should return first that succeeds"

testFixedStringSucceed = ((route (fixedString "posts/") alwaysSucceedView1 [] $ req1)
                          >>= return . (== (Just resp1)))
                         ~? "fixedString should leave view as is if the path matches completely"

testFixedStringFail = ((route (fixedString "bar/") alwaysSucceedView1 [] $ req1)
                          >>= return . isNothing)
                         ~? "fixedString should return Nothing if the path does not match"

testRouteToAnyPath = ((route anyPath alwaysSucceedView1 [] $ req1)
                      >>= return . (== Just resp1))
                     ~? "routeTo leaves a view alone if matcher always succeeds"

testRouteToNotAllMatched = ((route (fixedString "po") alwaysSucceedView1 [] $ req1)
                            >>= return . isNothing)
                           ~? "routeTo does not route to a view if the match does not exhaust the path"

routes = [
           empty                                  //-> alwaysSucceedView1         $ []
         , "posts/" <+/> empty                    //-> alwaysSucceedView2         $ []
         , intParam                               //-> viewWithIntParam1          $ []
         , stringParam                            //-> viewWithStringParam1       $ []
         , intParam </+> "test/"                  //-> viewWithIntParam2          $ []
         , "test/" <+/> intParam                  //-> viewWithIntParam2          $ []
         -- NB line below has to come after 'intParam </+> "test/"' line
         , anyParam </> stringParam               //-> viewWithIntAndStringParam1 $ []
         , intParam </> stringParam </> intParam  //-> viewWithIntStringInt1      $ []
         ]

testRoutes1 = (do
                Just resp <- dispatchRequest routes (mkGetReq "1/")
                return $ content resp == "Got integer: 1")
               ~? "Testing int parameter dispatch"

testRoutes2 = (do
                Just resp <- dispatchRequest routes (mkGetReq "1/test/")
                return $ content resp == "2: Got integer: 1")
               ~? "Testing int parameter dispatch with fixed string"

testRoutes3 = (do
                Just resp <- dispatchRequest routes (mkGetReq "1/Joe/3/")
                return $ content resp == "Got integer 1: 1 and string: Joe and integer 2: 3")
               ~? "Testing multiparameter dispatch"

testRoutes4 = (do
                Just resp <- dispatchRequest routes (mkGetReq "10/foo/")
                return $ content resp == "Got integer: 10 and string: foo")
               ~? "Testing stringParam dispatch"

testRoutes5 = (do
                Just resp <- dispatchRequest routes (mkGetReq "test/20/")
                return $ content resp == "2: Got integer: 20")
               ~? "Testing fixed string with integer"

testRoutes6 = (do
                Just resp <- dispatchRequest routes (mkGetReq "posts/")
                return $ resp == resp2)
               ~? "Testing fixed string with empty"

testRoutes7 = (do
                Just resp <- dispatchRequest routes (mkGetReq "")
                return $ resp == resp1)
               ~? "Testing empty matcher"

tests = test [
         testDispatchRequest1
        , testDispatchRequest2
        , testDispatchRequest3
        , testFixedStringSucceed
        , testFixedStringFail
        , testRouteToAnyPath
        , testRouteToNotAllMatched
        , testRoutes1
        , testRoutes2
        , testRoutes3
        , testRoutes4
        , testRoutes5
        , testRoutes6
        , testRoutes7
        ]
