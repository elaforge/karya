-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Note calls that transform other note calls.  They rely on track slicing
-- via 'Sub.sub_events'.
module Derive.Call.Prelude.Parent where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("ap", c_ap)
    , ("t", c_tuplet)
    , ("tup", c_tuplet) -- longer name in case 't' is shadowed
    , ("`arp-up`", c_real_arpeggio ToRight)
    , ("`arp-down`", c_real_arpeggio ToLeft)
    , ("`arp-rnd`", c_real_arpeggio Random)
    , ("interpolate", c_interpolate)
    , ("cycle", c_cycle)
    , ("cycle-t", c_cycle_t)
    ]

c_ap :: Derive.Generator Derive.Note
c_ap = Derive.generator Module.prelude "ap" Tags.subs
    "Derive sub events with no changes.  This is used to apply a transformer\
    \ to sub events."
    $ Sig.call0 $ Sub.derive . concat <=< Sub.sub_events

-- * tuplet

c_tuplet :: Derive.Generator Derive.Note
c_tuplet = Derive.generator Module.prelude "tuplet" Tags.subs
    "A generalized tuplet. The notes within its scope are stretched so that\
    \ their collective duration is the same as the tuplet's duration.\
    \\nThis doesn't work so well for zero duration notes. The last note\
    \ winds up at the end of the tuplet, which is not very useful. But zero\
    \ duration is common for percussion, so there's a hack to cover this case:\
    \ if there are >1 equidistant zero duration sub events, the distance\
    \ between them is considered their implicit duration.\
    \\nIf there are multiple note tracks, they get the stretch of the longest\
    \ one. This is so the timing will come out right if some of the notes are\
    \ chords."
    $ Sig.call0 $ \args -> Ly.when_lilypond
        (lily_tuplet (Args.range args) =<< Sub.sub_events args)
        (tuplet (Args.range args) =<< Sub.sub_events args)

tuplet :: (ScoreTime, ScoreTime) -> [[Sub.Event]] -> Derive.NoteDeriver
tuplet (start, end) tracks =
    case tuplet_note_end (map (map to_start_dur) tracks) of
        Nothing -> return mempty
        Just note_end ->
            mconcat $ map (Sub.fit (start, note_end) (start, end)) tracks
    where
    to_start_dur e = (Sub.event_start e, Sub.event_duration e)

-- | Get the end of the notes inside the tuplet.  If it has >1 note, and they
-- are all zero dur, and notes are equidistant, assume the last one has the
-- same dur.
tuplet_note_end :: (Ord a, Num a) => [[(a, a)]] -> Maybe a
tuplet_note_end = Seq.maximum . mapMaybe last_end
    where
    last_end events = infer_duration events <|> Seq.maximum (map end_of events)
    infer_duration events = case zipWith (-) (drop 1 starts) starts of
        d : ds | all ((==0) . dur_of) events && all (==d) ds ->
            Just $ start_of (last events) + d
        _ -> Nothing
        where starts = map start_of events
    start_of = fst
    dur_of = snd
    end_of (s, d) = s + d

-- | Emit a special 'tuplet_event' and derive the sub events with no time
-- changes.
lily_tuplet :: (ScoreTime, ScoreTime) -> [[Sub.Event]] -> Derive.NoteDeriver
lily_tuplet (start, end) track_notes = do
    track_notes <- case filter (not . null) track_notes of
        [] -> Derive.throw "no sub events"
        notes -> return notes
    track_events <- mapM Sub.derive track_notes
    let notes = map (filter (not . Ly.is_code0) . Stream.events_of) track_events
    notes_end <- case filter (not . null) notes of
        [] -> Derive.throw "no sub events"
        tracks -> Derive.require "can't figure out tuplet duration" $
            tuplet_note_end (map (map to_start_dur) tracks)
    real_start <- Derive.real start
    real_end <- Derive.real end
    tuplet <- Derive.place start (end-start) $
        tuplet_event (notes_end - real_start) (real_end - real_start)
    return $ mconcat (tuplet : track_events)
    where to_start_dur e = (Score.event_start e, Score.event_duration e)

-- | Emit the special event that tells lilypond to gather the following events
-- into a tuplet.
tuplet_event :: RealTime -> RealTime -> Derive.NoteDeriver
tuplet_event score_dur real_dur =
    Call.add_flags Flags.ly_code $
        Derive.with_environ (Constants.set_tuplet score_dur real_dur) $
        Call.note

-- * arpeggio

-- | Direction in which to arpeggiate.  This is a general arpeggiation that
-- just makes each track slightly delayed with regard to its neighbor.
--
-- Since I can't know the pitch of things (and a 'Sub.Event' may not have
-- a single pitch), the arpeggiation is by track position, not pitch.
data Arpeggio = ToRight | ToLeft | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.Generator Derive.Note
c_real_arpeggio arp = Derive.generator Module.prelude "arp" Tags.subs
    ("Arpeggiate the transformed notes. This shifts each note's start time\
    \ by a different amount, increasing to the right for `arp-up`,\
    \ to the left for `arp-down`, and randomly for `arp-rnd`.\
    \ Since it transforms score and not events, it doesn't know the\
    \ pitches of the sub notes (they may not have a single pitch) so\
    \ it's not actually \"up\" or \"down\"."
    ) $ Sig.call ((,)
    <$> Sig.defaulted "time" 0.1 "This much RealTime between each note."
    <*> Sig.defaulted "random" 0.5
        "Each note can vary randomly by `+- time/2 * random`."
    ) $ \(time, random) args -> lily_code args $
        arpeggio arp (RealTime.seconds time) random =<< Sub.sub_events args
    where
    lily_code = Ly.notes_with $ Ly.add_first (Ly.prepend, prefix)
        . Ly.add_first (Ly.append Constants.First, suffix)
    prefix = case arp of
        ToRight -> "\\arpeggioArrowUp"
        ToLeft -> "\\arpeggioArrowDown"
        Random -> "\\arpeggioNormal"
    suffix = "\\arpeggio"

-- | Shift each track of notes by a successive amount.
arpeggio :: Arpeggio -> RealTime -> Double -> [[Sub.Event]]
    -> Derive.NoteDeriver
arpeggio arp time random tracks = do
    delay_tracks <- jitter . zip (Seq.range_ 0 time) =<< sort tracks
    events <- fmap concat $ forM delay_tracks $ \(delay, track) ->
        forM track $ \(Sub.Event start dur d) -> do
            delay <- Call.score_duration start delay
            return $ Sub.Event (start+delay) (dur-delay) d
    Sub.derive events
    where
    jitter tracks
        | random == 0 = return tracks
        | otherwise = do
            rs <- Call.randoms_in (-random) random
            return $ zipWith nudge rs tracks
    nudge r (delay, notes)
        | delay == 0 = (delay, notes)
        | otherwise = (delay + time/2 * RealTime.seconds r, notes)
    sort = case arp of
        ToRight -> return
        ToLeft -> return . reverse
        Random -> Call.shuffle

-- | This is the old version that shifts each note as a postproc.  This means
-- it can arpeggiate by pitch since it knows the pitches at that point, but
-- also means it won't place events that consist of multiple notes correctly.
--
-- It's also buggy for events after the start since it will make their
-- duration negative.
arpeggio_by_note :: Arpeggio -> RealTime -> Derive.NoteDeriver
    -> Derive.NoteDeriver
arpeggio_by_note arp time deriver = do
    (events, logs) <- Stream.partition <$> deriver
    let sort = case arp of
            ToRight -> return . Seq.reverse_sort_on Score.initial_nn
            ToLeft -> return . Seq.sort_on Score.initial_nn
            Random -> Call.shuffle
    arpeggiated <- zipWith (Score.move_start 0) (Seq.range_ 0 time)
        <$> sort events
    return $ Stream.merge_logs logs $ Stream.from_sorted_events arpeggiated

-- * interpolate

c_interpolate :: Derive.Generator Derive.Note
c_interpolate = Derive.generator Module.prelude "interpolate" Tags.subs
    "Interpolate between multiple sub-tracks, each of which must have the\
    \ same number of events. This interpolates rhythm only. To interpolate\
    \ pitch and controls, it would need to work at the score event level,\
    \ rather than ui events."
    $ Sig.call (Sig.defaulted "at" (Sig.control "at" 0) "interpolate position")
    $ \at args -> do
        at <- Call.to_function at
        tracks <- filter (not . null) <$> Sub.sub_events args
        unless (all_equal (map length tracks)) $
            Derive.throw $ "sub tracks should have the same number of events: "
                <> pretty (map length tracks)
        to_real <- Derive.real_function
        Sub.derive $ interpolate_tracks (at . to_real) (Seq.rotate tracks)

interpolate_tracks :: (ScoreTime -> Signal.Y) -> [[Sub.Event]] -> [Sub.Event]
interpolate_tracks at = mapMaybe interpolate1
    where
    interpolate1 events = interpolate_events (at start) events
        where
        start = sum (map Sub.event_start events) / fromIntegral (length events)

interpolate_events :: Double -> [Sub.GenericEvent a]
    -> Maybe (Sub.GenericEvent a)
interpolate_events at events = case drop i events of
    [] -> Seq.last events
    [event] -> Just event
    e1 : e2 : _ -> Just $ Sub.Event
        { Sub.event_start =
            interpolate (Sub.event_start e1) (Sub.event_start e2)
        , Sub.event_duration =
            interpolate (Sub.event_duration e1) (Sub.event_duration e2)
        , Sub.event_note = Sub.event_note e1
        }
    where
    (i, frac) = properFraction (at * fromIntegral (length events - 1))
    interpolate x y = Num.scale x y (ScoreTime.double frac)

all_equal :: Eq a => [a] -> Bool
all_equal [] = True
all_equal (x:xs) = all (==x) xs

-- * cycle

c_cycle :: Derive.Generator Derive.Note
c_cycle = Derive.generator Module.prelude "cycle" Tags.subs
    "Apply transformers in a cycle to the sub events."
    $ Sig.call (
        Sig.many1 "transformer" "Transformers to apply."
    ) $ \transformers args -> do
        tracks <- Sub.sub_events args
        mconcatMap (Sub.derive . cycle_call (Args.context args) transformers)
            tracks

cycle_call :: Derive.Context Score.Event -> NonEmpty BaseTypes.Quoted
    -> [Sub.Event] -> [Sub.Event]
cycle_call ctx transformers =
    zipWith apply (cycle (NonEmpty.toList transformers))
    where apply quoted = fmap $ Eval.eval_quoted_transformers ctx quoted

c_cycle_t :: Derive.Generator Derive.Note
c_cycle_t = Derive.generator Module.prelude "cycle-t" Tags.subs
    "Apply transformers in a cycle to the sub events. This is like 'cycle',\
    \ except that it also gets a duration for each transformer, and cycles in\
    \ the given rhythm, rather than for each event."
    $ Sig.call (
        Sig.many1_pairs "transformer" "Transformers to apply, and the ScoreTime\
            \ duration for each transformer."
    ) $ \transformers args -> do
        tracks <- Sub.sub_events args
        let ctx = Args.context args
        mconcatMap (Sub.derive . cycle_t ctx (Args.start args) transformers)
            tracks

cycle_t :: Derive.Context Score.Event -> ScoreTime
    -> NonEmpty (BaseTypes.Quoted, Double) -> [Sub.Event] -> [Sub.Event]
cycle_t ctx start transformers =
    go (zip (cycle ts) (tail (scanl (+) start (cycle durs))))
    where
    go [] _ = []
    go _ [] = []
    go ts@((quoted, until) : rest_ts) (event : events)
        | Sub.event_start event >= until = go rest_ts (event : events)
        | otherwise = fmap (Eval.eval_quoted_transformers ctx quoted) event
            : go ts events
    (ts, durs) = second (map ScoreTime.double)
        (unzip (NonEmpty.toList transformers))
