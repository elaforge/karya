-- | Note calls that transform other note calls.
module Derive.Call.NoteTransformer where
import Util.Control
import qualified Util.Seq as Seq
import Ui
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional)
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("t", c_tuplet)
    , ("`arp-up`", c_real_arpeggio Up)
    , ("`arp-down`", c_real_arpeggio Down)
    , ("`arp-rnd`", c_real_arpeggio Random)
    ]

-- * tuplet

-- | This is a generalized tuplet.  The notes within its scope will be
-- stretched so that their collective length is the same as the tuplet.
-- If there are multiple note tracks, they will be stretched individually,
c_tuplet :: Derive.NoteCall
c_tuplet = Derive.stream_generator "tuplet" $ Note.place . stretched_tracks
    where
    stretched_tracks args =
        sort $ concatMap (stretched start end) (Note.sub_events args)
        where (start, end) = Derive.passed_range args
    stretched s e events = map stretch (sort events)
        where
        event_end = Seq.maximum (map (\(off, dur, _) -> off + dur) events)
        factor = (e-s) / maybe 1 (subtract s) event_end
        stretch (off, stretch, d) =
            ((off-s) * factor + s, stretch*factor, d)
    sort = Seq.sort_on (\(s, _, _) -> s)


data Arpeggio = Down | Up | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.NoteCall
c_real_arpeggio arp = Derive.stream_generator "arpeggio" $ \args ->
    CallSig.call1 args (optional "time" 0.1) $ \time ->
        arpeggio arp (RealTime.seconds time)
            (Note.place (concat (Note.sub_events args)))

-- | Shift each note by a successive amount.
arpeggio :: Arpeggio -> RealTime -> Derive.EventDeriver -> Derive.EventDeriver
arpeggio arp time deriver = do
    (events, logs) <- LEvent.partition <$> deriver
    let sort = case arp of
            Up -> return . Seq.sort_on Score.initial_pitch
            Down -> return . Seq.reverse_sort_on Score.initial_pitch
            Random -> Util.shuffle
    arpeggiated <- zipWith Score.move_start (Seq.range_ 0 time) <$> sort events
    return $ map LEvent.Log logs ++ map LEvent.Event arpeggiated
