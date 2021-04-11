-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Styled_test where
import           Data.Text (Text)

import qualified Util.Styled as Styled
import           Util.Styled (bg, black, bold, bright, fg, red, underline)

import           Util.Test


test_toHtml :: Test
test_toHtml = do
    let f = Styled.toHtml
    equal (f (Styled.plain (t "hi"))) "hi"
    equal (f (bold (t "hi"))) "<b>hi</b>"
    -- Identical styles are combined.
    equal (f (bold (t "hi") <> bold (t "there"))) "<b>hithere</b>"
    -- Outer style overrides inner one.
    equal (f (fg black (fg red (t "black"))))
        "<span style=\"color:black\">black</span>"
    equal (f (bold ("hi" <> underline (t "there"))))
        "<b>hi</b><b><u>there</u></b>"
    equal (f (fg (Styled.rgb 1 0.5 0) (t "rg")))
        "<span style=\"color:#ff8000\">rg</span>"
    equal (f (fg red (t "red")))
        "<span style=\"color:darkred\">red</span>"
    equal (f (bg black (t "black")))
        "<span style=\"background-color:black\">black</span>"
    -- nested spans are joined
    equal (f (bg red (fg (bright black) (t "txt"))))
        "<span style=\"color:darkgray;background-color:darkred\">txt</span>"


t :: Text -> Text
t = id
