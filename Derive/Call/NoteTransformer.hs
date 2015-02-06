-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Note calls that transform other note calls.  They rely on track slicing
-- via 'Sub.sub_events'.
module Derive.Call.NoteTransformer where
import qualified Control.Monad.Error as Error
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (defaulted)

import qualified Perform.Lilypond as Lilypond
import qualified Perform.RealTime as RealTime
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("ap", c_ap)
    , ("t", c_tuplet)
    , ("tup", c_tuplet) -- longer name in case 't' is shadowed
    , ("`arp-up`", c_real_arpeggio ToRight)
    , ("`arp-down`", c_real_arpeggio ToLeft)
    , ("`arp-rnd`", c_real_arpeggio Random)
    ]
    []


c_ap :: Derive.Generator Derive.Note
c_ap = Derive.make_call Module.prelude "ap" Tags.subs
    "Derive sub events with no changes.  This is used to apply a transformer\
    \ to sub events."
    $ Sig.call0 $ Sub.derive . concat <=< Sub.sub_events

-- * tuplet

c_tuplet :: Derive.Generator Derive.Note
c_tuplet = Derive.make_call Module.prelude "tuplet" Tags.subs
    "A generalized tuplet. The notes within its scope are stretched so that\
    \ their collective duration is the same as the tuplet's duration.\
    \\nThis doesn't work so well for zero duration notes. The last note\
    \ winds up at the end of the tuplet, which is not very useful. But zero\
    \ duration is common for percussion, so there's a hack to cover this case:\
    \ if there are >1 equidistant zero duration sub events, the distance\
    \ between them is considered their implicit duration.\
    \\nIf there are multiple note tracks, they are stretched independently."
    $ Sig.call0 $ \args -> lily_tuplet args $
        concatMapM (tuplet (Args.range args)) =<< Sub.sub_events args

tuplet :: (ScoreTime, ScoreTime) -> [Sub.Event] -> Derive.NoteDeriver
tuplet range events = case infer_duration of
    Nothing -> Sub.fit_to_range range events
    Just dur ->
        fit_to_duration range (dur * fromIntegral (length events)) events
    where
    -- If it has >1 note, and they are all zero dur, and notes are
    -- equidistant, assume the last one has the same dur.
    infer_duration = case zipWith (-) (drop 1 starts) starts of
        d : ds | all ((==0) . Sub.event_duration) events && all (==d) ds ->
            Just d
        _ -> Nothing
    starts = map Sub.event_start events

fit_to_duration :: (ScoreTime, ScoreTime) -> ScoreTime -> [Sub.Event]
    -> Derive.NoteDeriver
fit_to_duration (start, end) dur notes = Derive.place start factor $
    Sub.derive [note { Sub.event_start = Sub.event_start note - note_start }
        | note <- notes]
    where
    factor = (end - start) / (note_end - note_start)
    note_start = fromMaybe 1 $ Seq.minimum (map Sub.event_start notes)
    note_end = dur + note_start

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
lily_tuplet :: Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
lily_tuplet args not_lily = Lily.when_lilypond_config lily not_lily
    where
    lily config = either err return =<< Error.runErrorT . check config
        =<< Sub.sub_events args
    check config notes = do
        notes <- case filter (not . null) notes of
            [] -> Error.throwError "no sub events"
            [[]] -> Error.throwError "no sub events"
            _ : _ : _ -> Error.throwError ">1 non-empty sub track"
            [notes] -> return notes
        events <- lift $ LEvent.write_logs
            =<< Sub.derive (map (Sub.place (Args.start args) 2) notes)
            -- Double the notes duration, since staff notation tuplets shorten
            -- notes.
        dur <- case filter (not . Lily.is_code0) events of
            [] -> Error.throwError "no sub events"
            [_] -> Error.throwError "just one event"
            e : es
                | all ((== dur e) . dur) es -> return (dur e)
                | otherwise -> Error.throwError $
                    "all event durations must be equal: "
                    <> Text.intercalate ", " (map (pretty . dur) (e:es))
                where dur = Score.event_duration
        (start, end) <- lift $ Args.real_range args
        tuplet_dur <- to_dur config "tuplet" (end - start)
        note_dur <- to_dur config "note" dur
        ly_notes <- lift $ Lily.eval_events config start events
        lift $ Lily.code (Args.extent args) $
            tuplet_code tuplet_dur note_dur
                (length (filter (not . Lily.is_code0) events)) ly_notes
    err msg = do
        Log.warn $ "can't convert to ly tuplet: " <> msg
        not_lily
    to_dur config msg t = maybe
        (Error.throwError $ msg <> " duration must be simple")
        return (Lily.is_duration config t)

tuplet_code :: Lilypond.Duration -> Lilypond.Duration -> Int -> [Lily.Note]
    -> Lily.Ly
tuplet_code tuplet_dur note_dur note_count notes =
    "\\times " <> showt (d tuplet_dur `div` d note_dur) <> "/"
        <> showt note_count <> " { " <> Text.unwords notes <> " }"
    where d = toInteger . Lilypond.dur_to_time

-- * arpeggio

-- | Direction in which to arpeggiate.  This is a general arpeggiation that
-- just makes each track slightly delayed with regard to its neighbor.
--
-- Since I can't know the pitch of things (and a 'Sub.Event' may not have
-- a single pitch), the arpeggiation is by track position, not pitch.
data Arpeggio = ToRight | ToLeft | Random deriving (Show)

c_real_arpeggio :: Arpeggio -> Derive.Generator Derive.Note
c_real_arpeggio arp = Derive.make_call Module.prelude "arpeggio" Tags.subs
    ("Arpeggiate the transformed notes. This shifts each note's start time\
    \ by a different amount, increasing to the right for `arp-up`,\
    \ to the left for `arp-down`, and randomly for `arp-rnd`.\
    \ Since it transforms score and not events, it doesn't know the\
    \ pitches of the sub notes (they may not have a single pitch) so\
    \ it's not actually \"up\" or \"down\"."
    ) $ Sig.call ((,)
    <$> defaulted "time" 0.1 "This much RealTime between each note."
    <*> defaulted "random" 0.5
        "Each note can vary randomly by `+- time/2 * random`."
    ) $ \(time, random) args -> lily_code args $
        arpeggio arp (RealTime.seconds time) random =<< Sub.sub_events args
    where
    lily_code = Lily.notes_with
        (Lily.prepend_code prefix . Lily.add_code (Lily.SuffixFirst, suffix))
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
            delay <- Util.score_duration start delay
            return $ Sub.Event (start+delay) (dur-delay) d
    Sub.derive events
    where
    jitter tracks
        | random == 0 = return tracks
        | otherwise = do
            rs <- Util.randoms_in (-random) random
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
arpeggio_by_note :: Arpeggio -> RealTime -> Derive.NoteDeriver
    -> Derive.NoteDeriver
arpeggio_by_note arp time deriver = do
    (events, logs) <- LEvent.partition <$> deriver
    let sort = case arp of
            ToRight -> return . Seq.reverse_sort_on Score.initial_nn
            ToLeft -> return . Seq.sort_on Score.initial_nn
            Random -> Util.shuffle
    arpeggiated <- zipWith (Score.move_start 0) (Seq.range_ 0 time)
        <$> sort events
    return $ map LEvent.Log logs ++ map LEvent.Event arpeggiated
