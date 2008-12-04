{-# LANGUAGE DisambiguateRecordFields #-}
module Tests.Ella.Forms.Widgets

where

import Test.HUnit
import qualified Text.XHtml as X
import Text.XHtml ((!))
import Ella.Forms.Base
import Ella.Forms.Widgets
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA

render :: (X.HTML a) => a -> String
render = X.showHtmlFragment . X.toHtml

fullTextInput =  TI.TextInput { value = "foo"
                              , size = Just 10
                              , maxlength = Just 20
                              , name = "aname"
                              , identifier = "anid"
                              }

testTextInputRender_1 = "<input type=\"text\" name=\"\" value=\"\" />" ~=? render TI.emptyTextInput
testTextInputRender_2 = "<input type=\"text\" name=\"foo\" value=\"foo\" />" ~=? (render $ TI.emptyTextInput { TI.value = "foo", TI.name = "foo" })
testTextInputRender_3 = "<input type=\"text\" name=\"aname\" value=\"foo\" id=\"anid\" maxlength=\"20\" size=\"10\" />" ~=? (render fullTextInput)
testTextInputAddAttributes_1 = "<input type=\"text\" name=\"\" value=\"\" class=\"foo\" />" ~=?
                               (X.showHtmlFragment $ (X.toHtml TI.emptyTextInput) ! [X.theclass "foo"])
testTextInputRender_4 = "<input type=\"text\" name=\"foo\" value=\"\" id=\"id_foo\" />" ~=? (render $ setId "id_foo" $ TI.emptyTextInput { TI.name = "foo" })

testLabelRender = "<label for=\"anid\">A Label</label>" ~=? (render $ makeLabel "A Label" fullTextInput)
testTextareaRender_1 = "<textarea name=\"\"></textarea>" ~=? render TA.emptyTextarea
testTextareaRender_2 = "<textarea name=\"test\" id=\"abc\" cols=\"3\">&lt;hmm&gt;</textarea>" ~=?
                       (render $ TA.Textarea { value = "<hmm>"
                                             , cols = Just 3
                                             , identifier = "abc"
                                             , rows = Nothing
                                             , name = "test"
                                             })

tests = test [ testTextInputRender_1
             , testTextInputRender_2
             , testTextInputRender_3
             , testTextInputAddAttributes_1
             , testTextInputRender_4
             , testLabelRender
             , testTextareaRender_1
             , testTextareaRender_2
             ]
