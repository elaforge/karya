-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Derive.ShowVal where
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Numeric

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.TextUtil as TextUtil

import Global


instance TextUtil.Textlike Doc where
    toText (Doc t) = t
    fromText = Doc

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

-- * Doc

-- | This is for CallDoc.  It's only here for 'doc' and 'doc_pretty', and
-- is otherwise re-exported from "Derive.Derive".
newtype Doc = Doc Text
    deriving (Eq, Ord, Show, Pretty.Pretty, Monoid, String.IsString)

-- | Show a val for inclusion into CallDoc.
doc :: ShowVal a => a -> Doc
doc a = Doc $ "`" <> show_val a <> "`"

-- | This probably doesn't belong here, but it's useful in the same contexts as
-- 'doc'.
doc_pretty :: Pretty.Pretty a => a -> Doc
doc_pretty a = Doc $ "`" <> pretty a <> "`"
