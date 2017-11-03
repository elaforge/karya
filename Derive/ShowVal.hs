-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'show_val' method turns haskell values back to tracklang expressions.
-- It's similar to the opposite of 'Derive.Typecheck.Typecheck'.
module Derive.ShowVal where
import qualified Data.Text as Text
import qualified Numeric

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import Global hiding (pretty)


-- | Instances of ShowVal can be turned back into tracklang syntax.  Everything
-- produced by show_val should be parseable by "Derive.ParseBs", except values
-- that have no literal syntax, such as VPitch.
--
-- At least one place that relies on this is 'Derive.Call.Note.inverting'.
class ShowVal a where
    show_val :: a -> Text

hex_prefix :: Text
hex_prefix = "`0x`"

-- | VNums have hex and decimal literals, and show_val produces the decimal
-- one.  So I need a way to produce the hex literal.
show_hex_val :: Double -> Text
show_hex_val n
    | -1 <= n && n <= 1 = (if n < 0 then "-" else "") <> hex_prefix
        <> if Text.length h == 1 then "0" <> h else h
    | otherwise = show_val n
    where h = txt $ Numeric.showHex (round (abs n * 0xff)) ""

is_hex_val :: Text -> Bool
is_hex_val = (hex_prefix `Text.isPrefixOf`)

-- | Show a val for inclusion into CallDoc.
doc :: ShowVal a => a -> Doc.Doc
doc = Doc.literal . show_val

-- Really these instances should go in Derive.ParseBs, but it imports
-- Derive.TrackLang, which needs them.

instance ShowVal a => ShowVal [a] where
    show_val [] = "(list)"
    show_val xs = "(list " <> Text.unwords (map show_val xs) <> ")"

instance ShowVal Int where
    show_val = showt

instance ShowVal Double where
    show_val = Num.showFloat 3

instance ShowVal a => ShowVal (Maybe a) where
    show_val Nothing = "Nothing"
    show_val (Just a) = show_val a

instance (ShowVal a, ShowVal b) => ShowVal (Either a b) where
    show_val = either show_val show_val

instance ShowVal Bool where
    show_val b = if b then "t" else "f"

-- | This should be the inverse of 'Derive.Parse.p_str' and
-- 'Derive.Parse.p_unquoted_str'.
instance ShowVal Text where
    show_val s
        | bare = s
        | otherwise = "'" <> Text.concatMap quote s <> "'"
        where
        bare = case Text.uncons s of
            Just (c, cs) -> is_unquoted_head c && Text.all is_unquoted_body cs
            Nothing -> False
        quote '\'' = "''"
        quote c = Text.singleton c

is_unquoted_head :: Char -> Bool
is_unquoted_head c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '-'

is_unquoted_body :: Char -> Bool
is_unquoted_body c = c /= ' ' && c /= '\t' && c /= '\n' && c /= '='
