module Tests.Ella.Forms.Widgets

where

import Test.HUnit
import qualified Text.XHtml as X
import Text.XHtml ((!))
import Ella.Forms.Widgets

render = X.showHtmlFragment . X.toHtml

testTextInputRender_1 = "<input type=\"text\" name=\"\" value=\"\" />" ~=? render emptyTextInput
testTextInputRender_2 = "<input type=\"text\" name=\"foo\" value=\"foo\" />" ~=? (render $ emptyTextInput { defaultVal = "foo", name = "foo" })
testTextInputRender_3 = "<input type=\"text\" name=\"aname\" value=\"foo\" id=\"anid\" maxlength=\"20\" size=\"10\" />" ~=?
                        (render $ TextInput { defaultVal = "foo"
                                            , size = Just 10
                                            , maxlength = Just 20
                                            , name = "aname"
                                            , identifier = Just "anid"
                                            })
testTextInputAddAttributes_1 = "<input type=\"text\" name=\"\" value=\"\" class=\"foo\" />" ~=?
                               (X.showHtmlFragment $ (X.toHtml emptyTextInput) ! [X.theclass "foo"])

testTextInputRender_4 = "<input type=\"text\" name=\"foo\" value=\"\" id=\"id_foo\" />" ~=? (render $ setId "id_foo" $ emptyTextInput { name = "foo" })

tests = test [ testTextInputRender_1
             , testTextInputRender_2
             , testTextInputRender_3
             , testTextInputAddAttributes_1
             , testTextInputRender_4
             ]
