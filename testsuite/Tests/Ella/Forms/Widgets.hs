module Tests.Ella.Forms.Widgets

where

import Test.HUnit
import qualified Text.XHtml as X
import Ella.Forms.Widgets

render = X.showHtmlFragment . X.toHtml

testTextInputRender_1 = "<input type=\"text\" value=\"\" />" ~=? render emptyTextInput
testTextInputRender_2 = "<input type=\"text\" value=\"foo\" />" ~=? (render $ emptyTextInput { defaultVal = "foo" })
testTextInputRender_3 = "<input type=\"text\" value=\"foo\" maxlength=\"20\" size=\"10\" />" ~=?
                        (render $ TextInput { defaultVal = "foo"
                                            , size = Just 10
                                            , maxlength = Just 20
                                            })


tests = test [ testTextInputRender_1
             , testTextInputRender_2
             , testTextInputRender_3
             ]
