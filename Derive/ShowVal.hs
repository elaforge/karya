-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- ShowVal.show_val
-- | The 'show_val' method turns haskell values back to tracklang expressions.
-- It should be the inverse of 'Derive.Typecheck.from_val' .
-- 'Derive.Parse.parse_val'.
module Derive.ShowVal where
import qualified Data.Char as Char
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Numeric

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import           Global hiding (pretty)


-- | Instances of ShowVal can be turned back into tracklang syntax.  Everything
-- produced by show_val should be parseable by "Derive.Parse", except values
-- that have no literal syntax, such as VPSignal.
--
-- At least one place that relies on this is 'Derive.Call.Note.inverting'.
class ShowVal a where
    showVal :: a -> Text
    -- This intentionally has redundant constraints, which correspond to
    -- Typecheck.TEnum.  Any old Showable is unlikely to be Typecheckable.
    default showVal :: (Show a, Enum a, Bounded a) => a -> Text
    showVal = Text.pack . map Char.toLower . show

hexPrefix :: Text
hexPrefix = "`0x`"

-- | VNums have hex and decimal literals, and show_val produces the decimal
-- one.  So I need a way to produce the hex literal.
showHexVal :: Double -> Text
showHexVal n
    | -1 <= n && n <= 1 = (if n < 0 then "-" else "") <> hexPrefix
        <> if Text.length h == 1 then "0" <> h else h
    | otherwise = showVal n
    where h = txt $ Numeric.showHex (round (abs n * 0xff)) ""

isHexVal :: Text -> Bool
isHexVal = (hexPrefix `Text.isPrefixOf`)

-- | Show a val for inclusion into CallDoc.
doc :: ShowVal a => a -> Doc.Doc
doc = Doc.literal . showVal

-- Really these instances should go in "Derive.Parse", but it imports
-- "Derive.DeriveT", which needs them.

instance ShowVal a => ShowVal [a] where
    showVal [] = "\"(list)"
    showVal xs = "\"(list " <> Text.unwords (map showVal xs) <> ")"
instance ShowVal a => ShowVal (Set a) where
    showVal = showVal . Set.toList

instance ShowVal Int where
    showVal = showt

instance ShowVal Double where
    showVal = Num.showFloat 3

instance ShowVal (Ratio.Ratio Int) where
    showVal r =
        showVal (Ratio.numerator r) <> "/" <> showVal (Ratio.denominator r)

instance ShowVal a => ShowVal (Maybe a) where
    -- This is a bit sketchy because while _ can mean Nothing, it actually
    -- means use the default, which may not be Nothing.
    showVal Nothing = "_"
    showVal (Just a) = showVal a

instance (ShowVal a, ShowVal b) => ShowVal (Either a b) where
    showVal = either showVal showVal

instance ShowVal Bool where
    showVal b = if b then "t" else "f"

-- | This should be the inverse of 'Derive.Parse.p_str' and
-- 'Derive.Parse.p_unquoted_str'.
instance ShowVal Text where
    showVal s
        | bare = s
        | otherwise = "'" <> Text.concatMap quote s <> "'"
        where
        bare = case Text.uncons s of
            Just (c, cs) -> isUnquotedHead c && Text.all isUnquotedBody cs
            Nothing -> False
        quote '\'' = "''"
        quote c = Text.singleton c

isUnquotedHead :: Char -> Bool
isUnquotedHead c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '-'

isUnquotedBody :: Char -> Bool
isUnquotedBody c = c /= ' ' && c /= '\t' && c /= '\n' && c /= '='
