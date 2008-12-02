module Tests.Ella.Forms.Widgets

where

import Test.HUnit
import qualified Text.XHtml as X
import Text.XHtml ((!))
import Ella.Forms.Widgets

render = X.showHtmlFragment . X.toHtml

testTextInputRender_1 = "<input type=\"text\" value=\"\" />" ~=? render emptyTextInput
testTextInputRender_2 = "<input type=\"text\" value=\"foo\" />" ~=? (render $ emptyTextInput { defaultVal = "foo" })
testTextInputRender_3 = "<input type=\"text\" value=\"foo\" maxlength=\"20\" size=\"10\" />" ~=?
                        (render $ TextInput { defaultVal = "foo"
                                            , size = Just 10
                                            , maxlength = Just 20
                                            })
testTextInputAddAttributes_1 = "<input type=\"text\" value=\"\" class=\"foo\" />" ~=?
                               (X.showHtmlFragment $ (X.toHtml emptyTextInput) ! [X.theclass "foo"])


tests = test [ testTextInputRender_1
             , testTextInputRender_2
             , testTextInputRender_3
             , testTextInputAddAttributes_1
             ]
