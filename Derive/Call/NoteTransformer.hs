-- | Note calls that transform other note calls.  They rely on track slicing
-- via 'Note.sub_events'.
module Derive.Call.NoteTransformer where
import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("t", c_tuplet)
    , ("`arp-up`", c_real_arpeggio ToRight)
    , ("`arp-down`", c_real_arpeggio ToLeft)
    , ("`arp-rnd`", c_real_arpeggio Random)
    ]

-- * tuplet

c_tuplet :: Derive.NoteCall
c_tuplet = Derive.stream_generator "tuplet"
    ("A generalized tuplet. The notes within its scope are stretched so that\
    \ their collective duration is the same as the tuplet's duration.\
    \\nIf there are multiple note tracks, they will all be stretched\
    \ the same amount."
    ) $ Sig.call0 $ \args ->
        Note.place_at (Args.range args) (concat (Note.sub_events args))


-- * arpeggio

-- | Direction in which to arpeggiate.  This is a general arpeggiation that
-- just makes each track slightly delayed with regard to its neighbor.
--
-- Since I can't know the pitch of things (and a 'Note.Event' may not have
-- a single pitch), the arpeggiation is by track position, not pitch.
data Arpeggio = ToRight | ToLeft | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.NoteCall
c_real_arpeggio arp = Derive.stream_generator "arpeggio"
    ("Arpeggiate the transformed notes. This shifts each note's start time\
    \ by a different amount, increasing to the right for `arp-up`,\
    \ to the left for `arp-down`, and randomly for `arp-rnd`.\
    \ Since it transforms score and not events, it doesn't know the\
    \ pitches of the sub notes (they may not have a single pitch) so\
    \ it's not actually \"up\" or \"down\"."
    ) $ Sig.call
    ( defaulted "time" 0.1 "This much RealTime between each note."
    ) $ \time args -> Lily.note_transformer args arpeggio_attrs $
        arpeggio arp (RealTime.seconds time) (Note.sub_events args)
    where
    arpeggio_attrs = case arp of
        ToRight -> Attrs.arpeggio <> Attrs.up
        ToLeft -> Attrs.arpeggio <> Attrs.down
        Random -> Attrs.arpeggio

-- | Shift each track of notes by a successive amount.
arpeggio :: Arpeggio -> RealTime -> [[Note.Event]] -> Derive.EventDeriver
arpeggio arp time tracks = do
    delay_tracks <- zip (Seq.range_ 0 time) <$> sort tracks
    events <- fmap concat $ forM delay_tracks $ \(delay, track) ->
        forM track $ \(Note.Event start dur d) -> do
            new_start <- Util.delay start delay
            return $ Note.Event new_start (dur - (new_start - start)) d
    Note.place events
    where
    sort = case arp of
        ToRight -> return
        ToLeft -> return . reverse
        Random -> Util.shuffle

-- | This is the old version that shifts each note as a postproc.  This means
-- it can arpeggiate by pitch since it knows the pitches at that point, but
-- also means it won't place events that consist of multiple notes correctly.
--
-- It's also buggy for events after the start since it will make their
-- duration negative.
arpeggio_by_note :: Arpeggio -> RealTime -> Derive.EventDeriver
    -> Derive.EventDeriver
arpeggio_by_note arp time deriver = do
    (events, logs) <- LEvent.partition <$> deriver
    let sort = case arp of
            ToRight -> return . Seq.reverse_sort_on Score.initial_nn
            ToLeft -> return . Seq.sort_on Score.initial_nn
            Random -> Util.shuffle
    arpeggiated <- zipWith Score.move_start (Seq.range_ 0 time) <$> sort events
    return $ map LEvent.Log logs ++ map LEvent.Event arpeggiated
