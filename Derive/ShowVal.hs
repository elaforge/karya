{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Derive.ShowVal where
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.RealTime as RealTime


-- | Instances of ShowVal can be turned back into tracklang syntax.  Everything
-- produced by show_val should be parseable by "Derive.ParseBs", except values
-- that have no literal syntax, such as VPitch.
--
-- At least one place that relies on this is 'Derive.Call.Note.inverting'.
class ShowVal a where
    show_val :: a -> String

-- Really these instances should go in Derive.ParseBs, but it imports
-- Derive.TrackLang, which needs them.

instance ShowVal Int where
    show_val = show

instance ShowVal Double where
    show_val = Pretty.show_float 3

instance ShowVal String where
    show_val s = "'" ++ Seq.replace1 '\'' "''" s ++ "'"

instance ShowVal ScoreTime.ScoreTime where
    show_val =
        (++ [ScoreTime.suffix]) . Pretty.show_float 3 . ScoreTime.to_double

instance ShowVal RealTime.RealTime where
    show_val =
        (++ [RealTime.suffix]) . Pretty.show_float 3 . RealTime.to_seconds
