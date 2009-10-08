{-# LANGUAGE DisambiguateRecordFields #-}
module Tests.Ella.Forms.Widgets

where

import Test.HUnit
import qualified Text.XHtml as X
import Text.XHtml ((!))
import Ella.Forms.Base
import Ella.Forms.Widgets
import qualified Ella.Forms.Widgets.OptionList as OL
import qualified Ella.Forms.Widgets.RadioButton as RB
import qualified Ella.Forms.Widgets.RadioButtonList as RBL
import qualified Ella.Forms.Widgets.TextInput as TI
import qualified Ella.Forms.Widgets.Textarea as TA

render :: (X.HTML a) => a -> String
render = X.showHtmlFragment . X.toHtml

fullTextInput =  TI.TextInput { value = "foo"
                              , size = Just 10
                              , maxlength = Just 20
                              , name = "aname"
                              , identifier = "anid"
                              , password = False
                              }

testTextInputRender_1 = "<input type=\"text\" name=\"\" value=\"\" />" ~=? render TI.emptyTextInput
testTextInputRender_2 = "<input type=\"text\" name=\"foo\" value=\"foo\" />" ~=? (render $ TI.emptyTextInput { TI.value = "foo", TI.name = "foo" })
testTextInputRender_3 = "<input type=\"text\" name=\"aname\" value=\"foo\" id=\"anid\" maxlength=\"20\" size=\"10\" />" ~=? (render fullTextInput)
testTextInputAddAttributes_1 = "<input type=\"text\" name=\"\" value=\"\" class=\"foo\" />" ~=?
                               (X.showHtmlFragment $ (X.toHtml TI.emptyTextInput) ! [X.theclass "foo"])
testTextInputRender_4 = "<input type=\"text\" name=\"foo\" value=\"\" id=\"id_foo\" />" ~=? (render $ setId "id_foo" $ TI.emptyTextInput { TI.name = "foo" })
testPasswordRender_1 = "<input type=\"password\" name=\"foo\" value=\"foo\" />" ~=? (render $ TI.emptyTextInput { TI.value = "foo", TI.name = "foo", TI.password = True })

testLabelRender = "<label for=\"anid\">A Label</label>" ~=? (render $ makeLabel "A Label" fullTextInput)
testTextareaRender_1 = "<textarea name=\"\"></textarea>" ~=? render TA.emptyTextarea
testTextareaRender_2 = "<textarea name=\"test\" id=\"abc\" cols=\"3\">&lt;hmm&gt;</textarea>" ~=?
                       (render $ TA.Textarea { value = "<hmm>"
                                             , cols = Just 3
                                             , identifier = "abc"
                                             , rows = Nothing
                                             , name = "test"
                                             })

testValTextInput = "xyz" ~=? (getVal $ setVal "xyz" TI.emptyTextInput)
testValTextarea = "abcd\nfoo" ~=? (getVal $ setVal "abcd\nfoo" TA.emptyTextarea)

testRadioButtonRender = "<input type=\"radio\" name=\"foo\" value=\"val1\" id=\"id_foo\" />" ~=?
                         (render $ RB.RadioButton { value = "val1"
                                                  , name = "foo"
                                                  , identifier = "id_foo"
                                                  , checked = False
                                                  })

testRadioButtonRender2 = "<input type=\"radio\" name=\"foo\" value=\"val1\" id=\"id_foo\" checked=\"checked\" />" ~=?
                          (render $ RB.RadioButton { value = "val1"
                                                   , name = "foo"
                                                   , identifier = "id_foo"
                                                   , checked = True
                                                   })

testRadioButtonListRender = ("<label>" ++
                             "<input type=\"radio\" name=\"foo\" value=\"val1\" id=\"id_foo_0\" checked=\"checked\" />" ++
                             " Label 1</label><br />" ++
                             "<label>" ++
                             "<input type=\"radio\" name=\"foo\" value=\"val2\" id=\"id_foo_1\" />" ++
                             " Label 2</label><br />") ~=?
                            (render $ RBL.RadioButtonList { selectedValue = "val1"
                                                          , name = "foo"
                                                          , identifier = "id_foo"
                                                          , values = ["val1", "val2"]
                                                          , captions = map X.toHtml ["Label 1", "Label 2"]
                                                          })

testOptionListRender = ("<select name=\"foo\" size=\"8\" id=\"id_foo\" multiple=\"multiple\">" ++
                        "<option value=\"val1\">Value 1</option>" ++
                        "<option value=\"val2\" selected=\"selected\">Value 2</option>" ++
                        "<option value=\"val3\">Value 3</option>" ++
                        "<option value=\"val4\" selected=\"selected\">Value 4</option>" ++
                        "</select>") ~=?
                       (render $ OL.OptionList { selectedValues = ["val2", "val4"]
                                               , name = "foo"
                                               , identifier = "id_foo"
                                               , values = ["val1", "val2", "val3", "val4"]
                                               , captions = ["Value 1", "Value 2", "Value 3", "Value 4"]
                                               , multiple = True
                                               , size = 8
                                               })


tests = test [ testTextInputRender_1
             , testTextInputRender_2
             , testTextInputRender_3
             , testTextInputAddAttributes_1
             , testTextInputRender_4
             , testPasswordRender_1
             , testLabelRender
             , testTextareaRender_1
             , testTextareaRender_2
             , testValTextInput
             , testValTextarea
             , testRadioButtonRender
             , testRadioButtonRender2
             , testRadioButtonListRender
             , testOptionListRender
             ]
