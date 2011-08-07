-- | Note calls that transform other note calls.
module Derive.Call.NoteTransformer where
import Util.Control
import qualified Util.Seq as Seq
import Ui
import qualified Derive.Call.Note as Note
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
    ]

-- * tuplet

c_tuplet :: Derive.NoteCall
c_tuplet = Derive.stream_generator "tuplet" $ \args ->
        Note.place (stretched args)
    where
    stretched args = map stretch $ Seq.sort_on (\(s, _, _) -> s) events
        where
        events = Note.sub_events args
        (start, end) = Derive.passed_range args
        event_end = Seq.maximum (map (\(off, dur, _) -> off + dur) events)
        factor = (end - start) / maybe 1 (subtract start) event_end
        stretch (off, _, d) = ((off-start) * factor + start, factor, d)


data Arpeggio = Down | Up | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.NoteCall
c_real_arpeggio arp = Derive.stream_generator "arpeggio" $ \args ->
    CallSig.call1 args (optional "time" 0.1) $ \time ->
        arpeggio arp (RealTime.seconds time)
            (Note.place [(p, 1, d) | (p, _, d) <- Note.sub_events args])

-- | Shift each note by a successive amount.
arpeggio :: Arpeggio -> RealTime -> Derive.EventDeriver -> Derive.EventDeriver
arpeggio arp time deriver = do
    (events, logs) <- LEvent.partition <$> deriver
    sort <- case arp of
        Up -> return $ Seq.sort_on Score.initial_pitch
        Down -> return $ Seq.reverse_sort_on Score.initial_pitch
        Random -> Derive.throw "Random arpeggio not supported yet"
    let arpeggiated = zipWith (\offset event -> Score.move_start offset event)
            (Seq.range_ 0 time) (sort events)
    return $ map LEvent.Log logs ++ map LEvent.Event arpeggiated
