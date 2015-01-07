-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-proc calls that impose a new kind of articulation.
module Derive.Call.Post.Rearticulate where
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("slur-n", c_slur_n)
    , ("slur-dur", c_slur_dur)
    ]

c_slur_n :: Derive.Transformer Derive.Note
c_slur_n = Derive.transformer Module.prelude "slur-n" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are of a fixed size."
    $ Sig.callt ((,)
    <$> Sig.required "group" "How many notes in a group."
    <*> ControlUtil.curve_time_env
    ) $ \(group, curve) _args deriver -> do
        srate <- Util.get_srate
        slur_n srate group curve <$> deriver

type Curve = (ControlUtil.Curve, RealTime)

slur_n :: RealTime -> Int -> Curve -> Derive.Events -> Derive.Events
slur_n srate group curve = go
    where
    go events = case first LEvent.partition $ split_events group events of
        (([], logs), _) -> map LEvent.Log logs
        ((e : es, logs), post) -> map LEvent.Log logs
            ++ LEvent.Event (slur srate curve e es) : go post

slur :: RealTime -> Curve -> Score.Event -> [Score.Event] -> Score.Event
slur srate curve event events = event
    { Score.event_duration = dur
    , Score.event_untransformed_pitch = pitch
    }
    where
    pitch = slur_pitch srate curve (bracket_pitch event)
        (map bracket_pitch events)
    dur = Score.event_end (fromMaybe event (Seq.last events))
        - Score.event_start event

slur_pitch :: RealTime -> Curve -> PitchSignal.Signal -> [PitchSignal.Signal]
    -> PitchSignal.Signal
slur_pitch srate (curve, time) sig sigs = merge (sig : sigs) transitions
    where
    -- The transition should override the neighboring signals.
    merge (p:ps) (i:is) = p <> i `PitchSignal.prepend` merge ps is
    merge ps [] = mconcat ps
    merge [] is = mconcat is -- Shouldn't happen.
    transitions = map (uncurry transition) (zip (sig : sigs) sigs)
    transition _ _ | time == 0 = mempty
    transition prev next = fromMaybe mempty $ do
        (x0, _) <- PitchSignal.head prev
        (x1, y1) <- PitchSignal.last prev
        (x2, y2) <- PitchSignal.head next
        (x3, _) <- PitchSignal.last next
        let mid a b = (a + b) / 2
        -- Don't allow the curve past the midpoint of the note.  This way the
        -- transition will become quicker to accomodate shorter notes.
        let start = max (mid x0 x1) (mid x1 x2 - time / 2)
            end = min (mid x2 x3) (mid x1 x2 + time / 2)
        return $ PitchUtil.interpolate_segment srate curve False start y1 end y2

bracket_pitch :: Score.Event -> PitchSignal.Signal
bracket_pitch event =
    bracket (Score.event_start event) (Score.event_end event) $
        Score.event_transformed_pitch event

-- | Ensure there are samples at the start and end times.
-- TODO move to Util.TimeVector?
bracket :: RealTime -> RealTime -> PitchSignal.Signal -> PitchSignal.Signal
bracket start end = set_end . set_start . PitchSignal.within start end
    where
    set_start sig = case PitchSignal.head sig of
        Just (x, y) | x < start -> PitchSignal.signal [(start, y)] <> sig
        _ -> sig
    set_end sig = case PitchSignal.last sig of
        Just (x, y)
            | RealTime.eq x end -> sig
            | otherwise -> sig <> PitchSignal.signal [(end, y)]
        Nothing -> sig

-- | 'splitAt' for LEvents.
split_events :: Int -> [LEvent.LEvent a]
    -> ([LEvent.LEvent a], [LEvent.LEvent a])
split_events _ [] = ([], [])
split_events n (e:es)
    | n <= 0 = ([], e : es)
    | otherwise = (e : pre, post)
    where (pre, post) = split_events (if LEvent.is_event e then n - 1 else n) es

c_slur_dur :: Derive.Transformer Derive.Note
c_slur_dur = Derive.transformer Module.prelude "slur-dur" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are by duration."
    $ Sig.callt ((,,)
    <$> (TrackLang.default_real <$> Sig.required "dur"
        "How long each group is.")
    <*> (TrackLang.default_real <$> Sig.defaulted "offset" (TrackLang.real 0)
        "Groups start at this time.")
    <*> ControlUtil.curve_time_env
    ) $ \(dur, offset, curve) args deriver -> do
        start <- Args.real_start args
        dur <- Util.real_duration start dur
        offset <- Util.real_duration start offset
        srate <- Util.get_srate
        slur_dur srate dur offset curve <$> deriver

slur_dur :: RealTime -> RealTime -> RealTime -> Curve -> Derive.Events
    -> Derive.Events
slur_dur srate dur offset curve levents =
    map LEvent.Log logs
        ++ map (LEvent.Event . apply) (group_dur dur offset events)
    where
    apply (e :| es) = slur srate curve e es
    (events, logs) = LEvent.partition levents

group_dur :: RealTime -> RealTime -> [Score.Event] -> [NonEmpty Score.Event]
group_dur dur offset = Seq.group_eq_on group_of
    where
    group_of = floor . RealTime.to_seconds . (/dur) . subtract offset
        . Score.event_start
