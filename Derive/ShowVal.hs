{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Derive.ShowVal where
import qualified Numeric

import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.RealTime as RealTime


-- | Instances of ShowVal can be turned back into tracklang syntax.  Everything
-- produced by show_val should be parseable by "Derive.ParseBs", except values
-- that have no literal syntax, such as VPitch.
--
-- At least one place that relies on this is 'Derive.Call.Note.inverting'.
class ShowVal a where
    show_val :: a -> String

hex_prefix :: String
hex_prefix = "`0x`"

show_hex_val :: Double -> String
show_hex_val n = hex_prefix ++ if length h == 1 then '0' : h else h
    where h = Numeric.showHex (round (n * 0xff)) ""

-- | Show a val for inclusion into CallDoc.
doc_val :: (ShowVal a) => a -> String
doc_val a = '`' : show_val a ++ "`"

-- Really these instances should go in Derive.ParseBs, but it imports
-- Derive.TrackLang, which needs them.

instance ShowVal Int where
    show_val = show

instance ShowVal Double where
    show_val = Pretty.show_float 3

instance ShowVal ScoreTime.ScoreTime where
    show_val =
        (++ [ScoreTime.suffix]) . Pretty.show_float 3 . ScoreTime.to_double

instance ShowVal RealTime.RealTime where
    show_val =
        (++ [RealTime.suffix]) . Pretty.show_float 3 . RealTime.to_seconds
