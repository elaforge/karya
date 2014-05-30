-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-processing to apply a string idiom.
module Derive.Call.Idiom.String where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("gliss-a", make_gliss "gliss-a" True)
    , ("gliss", make_gliss "gliss" False)
    ]
    [ ("bent-string", c_bent_string)
    , ("stopped-string", c_stopped_string)
    ]

module_ :: Module.Module
module_ = "idiom" <> "string"

c_bent_string :: Derive.Transformer Derive.Note
c_bent_string = Derive.transformer module_ "bent-string"
    (Tags.postproc <> Tags.inst)
    ("Post-process events to play in a monophonic string-like idiom, where\
    \ strings must be bent or stopped to reach non-open pitches.\
    \ Originally it was meant to play in the style of a 古箏 or\
    \ other zither, but may also be appropriate for stopped strings\
    \ like the violin family. This only makes sense for instruments with some\
    \ decay, since otherwise you can't hear the transitions.\
    \ Further documentation is in 'Derive.Call.Idiom.String'."
    ) $ Sig.callt ((,,,)
    <$> Sig.defaulted "attack" (control "string-attack" 0.1)
        "Time for a string to bend to its desired pitch. A fast attack\
        \ sounds like a stopped string."
    <*> Sig.defaulted "release" (control "string-release" 0.1)
        "Time for a string to return to its original pitch."
    <*> Sig.defaulted "delay" (control "string-delay" 0)
        "If the string won't be used for the following note, it will be\
        \ released after this delay."
    <*> open_strings_env
    ) $ \(attack, release, delay, open_strings) _args deriver -> do
        srate <- Util.get_srate
        let linear = PitchUtil.interpolator srate id
        string_idiom linear linear open_strings attack delay release =<< deriver

c_stopped_string :: Derive.Transformer Derive.Note
c_stopped_string = Derive.transformer module_ "stopped-string"
    (Tags.postproc <> Tags.inst)
    "A specialization of `bent-string` but for stopped strings, like the\
    \ violin family, where strings instantly jump to their pitches."
    $ Sig.callt ((,)
    <$> Sig.defaulted "delay" (control "string-delay" 0)
        "String release delay time."
    <*> open_strings_env
    ) $ \(delay, open_strings) _args deriver -> do
        srate <- Util.get_srate
        events <- deriver
        let attack = TrackLang.constant_control 0
            release = TrackLang.constant_control 0
        let linear = PitchUtil.interpolator srate id
        string_idiom linear linear open_strings attack delay release events

open_strings_env :: Sig.Parser [PitchSignal.Pitch]
open_strings_env = Sig.check non_empty $
    Sig.environ "open-strings" Sig.Unprefixed []
        "Space separated list of the pitches of the open strings."
    where
    non_empty xs
        | null xs = Just "open-strings required"
        | otherwise = Nothing

{- | Post-process events to play them in a monophonic string-like idiom.

    This tweaks the ends of the pitch signals of notes.  When a new note is
    played, the next event is examined to determine if it will share a string
    or not.

    If the string must be used for the following note, the end of the event is
    bent up to the next pitch before the next event is triggered.  This is
    called the \"attack\".  A fast attack gives the sound of a stopped string,
    a slow one sounds like a bent one.

    If the string won't be used for the following note, it will be released
    after a delay.  The release time determines how long it will take to reach
    its open pitch.  Since the release happens after the note ends, only
    instruments with a bit of decay will have an audible release.

    This does't do anything fancy like simulate hand position or alternate
    fingerings.

    TODO It would be possible to have a polyphonic effect by allowing more than
    one stopped string at a time.
-}
string_idiom ::
    PitchUtil.Interpolator -- ^ interpolator to draw the attack curve
    -> PitchUtil.Interpolator -- ^ draw the release curve
    -> [PitchSignal.Pitch] -- ^ Pitches of open strings.
    -> TrackLang.ValControl -- ^ Attack time.
    -> TrackLang.ValControl -- ^ Release delay.
    -> TrackLang.ValControl -- ^ Time for string to return to its open pitch.
    -> Derive.Events -> Derive.NoteDeriver
string_idiom attack_interpolator release_interpolator open_strings attack delay
        release all_events = Post.event_head all_events $ \event events -> do
    open_nns <- mapM Pitches.pitch_nn open_strings
    let strings = Map.fromList (zip open_nns open_strings)
    case initial_state strings event of
        Nothing -> Derive.throw $ "initial pitch below lowest string: "
            ++ show (Score.initial_nn event)
        Just state -> do
            attack <- Post.control id attack events
            delay <- Post.control id delay events
            release <- Post.control id release events
            (final, result) <- Post.map_events_asc_m (go strings) state
                (LEvent.zip4 attack delay release events)
            return $! result ++ [LEvent.Event $ state_event final]
    where
    go strings state (attack, delay, release, event) = do
        start <- Derive.score (Score.event_start event)
        let dur = Util.typed_real_duration Util.Real start
        attack <- dur attack
        delay <- dur delay
        release <- dur release
        process strings attack_interpolator release_interpolator
            (attack, delay, release) state event

-- | Monophonic:
--
-- - If the note falls on a new string, release the previously playing note
-- (bend down to its open position) and emit it.
--
-- - If the note falls on the string in use, bend that string up to the note
-- to be played and emit it.
process :: Map.Map Pitch.NoteNumber PitchSignal.Pitch
    -- ^ The strings are tuned to Pitches, but to compare Pitches I have to
    -- evaluate them to NoteNumbers first.
    -> PitchUtil.Interpolator -> PitchUtil.Interpolator
    -> (RealTime, RealTime, RealTime) -> State -> Score.Event
    -> Derive.Deriver (State, [Score.Event])
process strings attack_interpolator release_interpolator
        (attack_time, delay_time, release_time)
        state@(State sounding_string prev) event = do
    pitch <- Derive.require "no pitch" $ Score.initial_pitch event
    nn <- Derive.require_right pretty $ PitchSignal.pitch_nn pitch
    case find_string nn strings of
        Nothing -> do
            Log.warn $ pretty nn ++ " below lowest string: " ++ pretty strings
            return (state, [])
        Just string -> do
            new_event <- Derive.require "missing pitches" (emit pitch string)
            return (State string event, [new_event])
    where
    start = Score.event_start event
    emit pitch string
        -- Re-use an already sounding string.
        | fst string == fst sounding_string = attack attack_interpolator
            attack_time pitch start prev
        -- Sounding string is no longer used, so it can be released.
        | otherwise = release release_interpolator delay_time release_time
            (snd sounding_string) start prev

-- | Bend the event up to the next note.
attack :: PitchUtil.Interpolator -> RealTime -> PitchSignal.Pitch -> RealTime
    -> Score.Event -> Maybe Score.Event
attack interpolator time pitch next_event event = do
    let start_x = next_event - time
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolator start_x start_pitch next_event
        pitch event

-- | After releasing a note, you release your hand, which means the pitch
-- should bend down to the open string.
release :: PitchUtil.Interpolator -> RealTime -> RealTime
    -> PitchSignal.Pitch -> RealTime -> Score.Event
    -> Maybe Score.Event
release interpolator delay time pitch next_event event = do
    let start_x = next_event + delay
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolator start_x start_pitch
        (start_x + time) pitch event

merge_curve :: PitchUtil.Interpolator -> RealTime -> PitchSignal.Pitch
    -> RealTime -> PitchSignal.Pitch -> Score.Event -> Score.Event
merge_curve interpolator x0 y0 x1 y1 event =
    event { Score.event_pitch = new_pitch }
    where
    curve = interpolator False x0 y0 x1 y1
    new_pitch = Score.event_pitch event <> curve

find_string :: Pitch.NoteNumber -> Map.Map Pitch.NoteNumber PitchSignal.Pitch
    -> Maybe (Pitch.NoteNumber, PitchSignal.Pitch)
find_string = Map.lookup_below

-- | Keep track of the current string state.
data State = State {
    -- | The string that is currently sounding.  This is the /open/ pitch.
    state_sounding :: !(Pitch.NoteNumber, PitchSignal.Pitch)
    -- | The previous event.  Since each event is modified based on the next
    -- pitch, each 'process' call is one event ahead of the actual event
    -- emitted.
    , state_event :: !Score.Event
    } deriving (Show)

initial_state :: Map.Map Pitch.NoteNumber PitchSignal.Pitch -> Score.Event
    -> Maybe State
initial_state strings event = do
    nn <- Score.initial_nn event
    string <- find_string nn strings
    return $ State
        { state_sounding = string
        , state_event = event
        }


-- * gliss

gliss :: [PitchSignal.Pitch] -> RealTime -> Signal.Y -> Signal.Y -> RealTime
    -> Derive.NoteDeriver
gliss pitches time start_dyn end_dyn end = do
    let dur = time / fromIntegral (length pitches)
        start = end - time
        ts = take (length pitches) (Seq.range_ start dur)
        dyns = map (Num.scale start_dyn end_dyn . RealTime.to_seconds
            . Num.normalize start end) ts
    score_ts <- mapM Derive.score ts
    score_dur <- Util.score_duration end dur
    let note (t, p, dyn) = Derive.place t score_dur $ Util.with_dynamic dyn $
            Util.pitched_note p
    mconcat $ map note $ zip3 score_ts pitches dyns

gliss_pitches :: [PitchSignal.Pitch] -> PitchSignal.Pitch -> Int
    -> Derive.Deriver [PitchSignal.Pitch]
gliss_pitches open_strings dest_pitch gliss_start = do
    dest_nn <- Pitches.pitch_nn dest_pitch
    -- TODO shouldn't need to eval them all
    open_nns <- mapM Pitches.pitch_nn open_strings
    let strings = Seq.sort_on snd $ zip open_strings open_nns
    -- 0 2 4 6 8 10
    return $ if gliss_start >= 0
        -- 5 -> 6 8 10 -> 10 8 6 5
        then reverse $ take gliss_start $ map fst $
            dropWhile ((<=dest_nn) . snd) strings
        -- 5 -> 0 2 4
        else Seq.rtake (-gliss_start) $ map fst $
            takeWhile ((<dest_nn) . snd) strings

-- | Gracelessly factor both forms of glissando.
--
-- The other option would be a single call with an environ to switch between
-- the two behaviours, and expect to bind locally.  But it seems like both
-- would be useful simultaneously, and why not have a reasonable default
-- vocabulary if I can manage it?
make_gliss :: Text -> Bool -> Derive.Generator Derive.Note
make_gliss name is_absolute = Derive.make_call module_ name mempty
    "Glissando along the open strings, taking an absolute amount of time."
    $ Sig.call ((,,,)
    <$> Sig.required "start"
        "Start this many strings above or below the destination pitch."
    <*> (if is_absolute
        then Sig.defaulted "time" (TrackLang.real 0.25)
            "Time in which to play the glissando."
        else Sig.defaulted "time" (TrackLang.real 0.075)
            "Time between each note.")
    <*> Sig.defaulted "dyn" Nothing "Start at this dyn, and interpolate\
        \ to the destination dyn. If not given, the dyn is constant."
    <*> open_strings_env
    ) $ \(gliss_start, time, maybe_start_dyn, open_strings) -> Sub.inverting $
    \args -> do
        end <- Args.real_start args
        time <- Util.real_duration end time
        dest_pitch <- Util.get_pitch end
        dest_dyn <- Util.dynamic end
        let start_dyn = fromMaybe dest_dyn maybe_start_dyn
        pitches <- gliss_pitches open_strings dest_pitch gliss_start
        gliss pitches
            (if is_absolute then time else time * fromIntegral (length pitches))
            start_dyn dest_dyn end
            <> Util.placed_note args
