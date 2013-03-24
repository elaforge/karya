-- | Note calls that transform other note calls.  They rely on track slicing
-- via 'Note.sub_events'.
module Derive.Call.NoteTransformer where
import qualified Control.Monad.Trans.Either as Either

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("ap", c_ap)
    , ("t", c_tuplet)
    , ("`arp-up`", c_real_arpeggio ToRight)
    , ("`arp-down`", c_real_arpeggio ToLeft)
    , ("`arp-rnd`", c_real_arpeggio Random)
    ]


c_ap :: Derive.NoteCall
c_ap = Derive.stream_generator "ap" Tags.subs
    "Derive sub events with no changes.  This is used to apply a transformer\
    \ to sub events."
    $ Sig.call0 $ Note.place . concat <=< Note.sub_events

-- * tuplet

c_tuplet :: Derive.NoteCall
c_tuplet = Derive.stream_generator "tuplet" Tags.subs
    "A generalized tuplet. The notes within its scope are stretched so that\
    \ their collective duration is the same as the tuplet's duration.\
    \\nIf there are multiple note tracks, they will all be stretched\
    \ the same amount."
    $ Sig.call0 $ \args -> emit_lily_tuplet args $
        Note.place_at (Args.range args) . concat =<< Note.sub_events args

-- | 'c_tuplet' works by lengthening notes to fit in its range, but staff
-- notation tuplets work by shortening notes.  So I double the duration
-- of the events and figure out how much to shrink them to get to the desired
-- duration.
--
-- Since the @t@ call is more flexible than a staff tuplet, this enforces
-- some restrictions.  Both the tuplet's notes and the tuplet as a whole must
-- be a 'Lilypond.Duration', and all the notes must be the same duration.  So
-- I don't support tuplets with ties and whatnot inside.  It's theoretically
-- possible, but seems hard.
emit_lily_tuplet :: Derive.PassedArgs d -> Derive.EventDeriver
    -> Derive.EventDeriver
emit_lily_tuplet args not_lily = Lily.when_lilypond lily not_lily
    where
    lily config = either err return =<< Either.runEitherT . check config
        =<< Note.sub_events args
    check config notes = do
        (note, notes) <- case filter (not . null) notes of
            [] -> Either.left $ Just "no sub events"
            [[]] -> Either.left $ Just "no sub events"
            _ : _ : _ -> Either.left $ Just ">1 non-empty sub track"
            [[_]] -> Either.left Nothing
            [n : ns]
                | not $ all ((== Note.event_duration n) . Note.event_duration)
                        ns ->
                    Either.left $ Just "all event durations must be equal"
                | otherwise -> Either.right (n, ns)
        (start, end) <- lift $ Args.real_range args
        tuplet_dur <- is_dur "tuplet" (end - start)
        real_dur <- lift $
            Util.real_dur (Args.start args) (Note.event_duration note)
        note_dur <- is_dur "note" (real_dur * 2)
        ly_notes <- lift $ Lily.eval config args $
            map (Note.stretch (Args.start args) 2) (note : notes)
        lift $ Lily.code (Args.extent args) $
            tuplet_code tuplet_dur note_dur (length notes + 1) ly_notes
        where
        is_dur msg t = maybe
            (Either.left $ Just $ msg ++ " duration must be simple")
            return (Lily.is_duration config t)
    err msg = do
        when_just msg $ Log.warn . ("can't convert to ly tuplet: "++)
        not_lily

tuplet_code :: Lilypond.Duration -> Lilypond.Duration -> Int -> [Lily.Note]
    -> String
tuplet_code tuplet_dur note_dur note_count notes =
    "\\times " <> show (d tuplet_dur `div` d note_dur) <> "/"
        <> show note_count <> " { " <> unwords notes <> " }"
    where d = toInteger . Lilypond.dur_to_time

-- * arpeggio

-- | Direction in which to arpeggiate.  This is a general arpeggiation that
-- just makes each track slightly delayed with regard to its neighbor.
--
-- Since I can't know the pitch of things (and a 'Note.Event' may not have
-- a single pitch), the arpeggiation is by track position, not pitch.
data Arpeggio = ToRight | ToLeft | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.NoteCall
c_real_arpeggio arp = Derive.stream_generator "arpeggio"
    (Tags.subs <> Tags.ornament)
    ("Arpeggiate the transformed notes. This shifts each note's start time\
    \ by a different amount, increasing to the right for `arp-up`,\
    \ to the left for `arp-down`, and randomly for `arp-rnd`.\
    \ Since it transforms score and not events, it doesn't know the\
    \ pitches of the sub notes (they may not have a single pitch) so\
    \ it's not actually \"up\" or \"down\"."
    ) $ Sig.call ((,)
    <$> defaulted "time" 0.1 "This much RealTime between each note."
    <*> defaulted "variation" 0.5
        "Each note can vary randomly by `+- time/2 * variation`."
    ) $ \(time, variation) args -> lily_code args $
        arpeggio arp (RealTime.seconds time) variation =<< Note.sub_events args
    where
    lily_code = Lily.notes_with
        (Lily.prepend_code prefix . Lily.add_code (Lily.SuffixFirst, suffix))
    prefix = case arp of
        ToRight -> "\\arpeggioArrowUp"
        ToLeft -> "\\arpeggioArrowDown"
        Random -> "\\arpeggioNormal"
    suffix = "\\arpeggio"

-- | Shift each track of notes by a successive amount.
arpeggio :: Arpeggio -> RealTime -> Double -> [[Note.Event]]
    -> Derive.EventDeriver
arpeggio arp time variation tracks = do
    delay_tracks <- jitter . zip (Seq.range_ 0 time) =<< sort tracks
    events <- fmap concat $ forM delay_tracks $ \(delay, track) ->
        forM track $ \(Note.Event start dur d) -> do
            new_start <- Util.delay start delay
            return $ Note.Event new_start (dur - (new_start - start)) d
    Note.place events
    where
    jitter tracks
        | variation == 0 = return tracks
        | otherwise = do
            rs <- Util.randoms_in (-variation) variation
            return $ zipWith nudge rs tracks
    nudge r (delay, notes)
        | delay == 0 = (delay, notes)
        | otherwise = (delay + time/2 * RealTime.seconds r, notes)
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
