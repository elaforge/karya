-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Derive.ShowVal where
import qualified Data.Text as Text
import qualified Numeric

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.RealTime as RealTime


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
show_hex_val n = hex_prefix <> if Text.length h == 1 then "0" <> h else h
    where h = txt $ Numeric.showHex (round (n * 0xff)) ""

is_hex_val :: Text -> Bool
is_hex_val = (hex_prefix `Text.isPrefixOf`)

-- | Show a val for inclusion into CallDoc.
doc_val :: (ShowVal a) => a -> Text
doc_val a = "`" <> show_val a <> "`"

-- Really these instances should go in Derive.ParseBs, but it imports
-- Derive.TrackLang, which needs them.

instance ShowVal Int where
    show_val = showt

instance ShowVal Double where
    show_val = txt . Pretty.show_float 3

instance ShowVal ScoreTime.ScoreTime where
    show_val = (`Text.snoc` ScoreTime.suffix) . txt . Pretty.show_float 3
        . ScoreTime.to_double

instance ShowVal RealTime.RealTime where
    show_val = (`Text.snoc` RealTime.suffix) . txt . Pretty.show_float 3
        . RealTime.to_seconds
