module Tests.Ella.Param where

import Test.HUnit
import Ella.Param

testIntParam = Just 123456 ~=? (capture "123456" :: Maybe Int)
testIntParamNoParse = Nothing ~=? (capture "foo" :: Maybe Int)
testIntParamOverflow = Nothing ~=? (capture "11111111111111" :: Maybe Int)


tests = test [ testIntParam
             , testIntParamNoParse
             , testIntParamOverflow
             ]
