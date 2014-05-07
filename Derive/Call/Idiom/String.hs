-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-processing to apply a string idiom.
module Derive.Call.Idiom.String where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (control, defaulted)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Types


-- | This is just a placeholder.  Any real use will want to put the
-- appropriate degrees from the appropriate scale in here, either via the
-- static config or some kind of tracklang list literal.
note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("string-guzheng", c_guzheng $ notes ["4c", "4d", "4e", "4g", "4a"])
    , ("string-viola", c_violin $ notes ["4c", "4g", "5d", "5a"])
    ]
    where notes = map (flip TrackLang.call [])

module_ :: Module.Module
module_ = "idiom" <> "string"

c_guzheng :: [TrackLang.PitchCall] -> Derive.Transformer Derive.Note
c_guzheng strings = Derive.transformer module_ "guzheng"
    (Tags.postproc <> Tags.inst)
    ("Post-process events to play in a monophonic string-like idiom, where\
    \ strings must be bent or stopped to reach non-open pitches.\
    \ Originally it was meant to play in the style of a 古箏 or\
    \ other zither, but may also be appropriate for stopped strings\
    \ like the violin family.  Further documentation is in\
    \ 'Derive.Call.Idiom.String'."
    ) $ Sig.callt ((,,)
    <$> defaulted "attack" (control "string-attack" 0.5)
        "Time for a string to bend to its desired pitch. A fast attack\
        \ sounds like a stopped string."
    <*> defaulted "release" (control "string-release" 0.5)
        "Time for a string to return to its original pitch."
    <*> defaulted "delay" (control "string-delay" 0)
        "If the string won't be used for the following note, it will be\
        \ released after this delay."
    ) $ \(attack, release, delay) _args deriver -> do
        -- TODO if I care about retuning notes I should pass a time
        string_pitches <- mapM (Eval.eval_pitch 0) strings
        srate <- Util.get_srate
        events <- deriver
        let linear = Call.Pitch.interpolator srate id
        string_idiom linear linear string_pitches attack delay release events

-- | A string idiom in the style of stopped strings like the violin family.
-- Strings instantly jump to their pitches.
c_violin :: [TrackLang.PitchCall] -> Derive.Transformer Derive.Note
c_violin strings = Derive.transformer module_ "violin"
    (Tags.postproc <> Tags.inst)
    "A specialization of `string-guzheng` for stopped strings." $
    Sig.callt
    ( defaulted "delay" (control "string-delay" 0) "String release delay time."
    ) $ \delay _args deriver -> do
        string_pitches <- mapM (Eval.eval_pitch 0) strings
        srate <- Util.get_srate
        events <- deriver
        let attack = TrackLang.constant_control 0
            release = TrackLang.constant_control 0
        let linear = Call.Pitch.interpolator srate id
        string_idiom linear linear string_pitches attack delay release events

-- | Post-process events to play them in a monophonic string-like idiom.
--
-- This tweaks the ends of the pitch signals of notes.  When a new note is
-- played, the next event is examined to determine if it will share a string
-- or not.
--
-- If the string must be used for the following note, the end of the event is
-- bent up to the next pitch before the next event is triggered.  This is
-- called the \"attack\".  A fast attack gives the sound of a stopped string,
-- a slow one sounds like a bent one.
--
-- If the string won't be used for the following note, it will be released
-- after a delay.  The release time determines how long it will take to reach
-- its open pitch.  Since the release happens after the note ends, only
-- instruments with a bit of decay will have an audible release.
--
-- This does't do anything fancy like simulate hand position or alternate
-- fingerings.
--
-- TODO this evaluates the open strings only once at the beginning of the
-- postprocessing.  So any kind of fancy retuning isn't going to happen.  To
-- do that I think I'd need some cooperation from the note call, to evaluate
-- and stash the pitches of the open strings at the point of the note
-- evaluation.  I might be able do that by modifying the null call.
--
-- TODO It would be possible to have a polyphonic effect by allowing more than
-- one stopped string at a time.
string_idiom ::
    Call.Pitch.Interpolator -- ^ interpolator to draw the attack curve
    -> Call.Pitch.Interpolator -- ^ draw the release curve
    -> [PitchSignal.Pitch] -- ^ Pitches of open strings.
    -> TrackLang.ValControl -- ^ Attack time.
    -> TrackLang.ValControl -- ^ Release delay.
    -> TrackLang.ValControl -- ^ Time for string to return to its open pitch.
    -> Derive.Events -> Derive.NoteDeriver
string_idiom attack_interpolator release_interpolator open_strings attack delay
        release all_events = Post.event_head all_events $ \event events -> do
    open_nns <- mapM Pitches.pitch_nn open_strings
    case initial_state (zip open_nns open_strings) event of
        Nothing -> Derive.throw $ "initial pitch below lowest string: "
            ++ show (Score.initial_nn event)
        Just state -> do
            attack <- Post.control id attack events
            delay <- Post.control id delay events
            release <- Post.control id release events
            (final, result) <- Post.map_events_asc_m go state
                (LEvent.zip4 attack delay release events)
            return $! result ++ [LEvent.Event $ state_event final]
    where
    go state (attack, delay, release, event) = do
        start <- Derive.score (Score.event_start event)
        let dur = Util.typed_real_duration Util.Real start
        attack <- dur attack
        delay <- dur delay
        release <- dur release
        process attack_interpolator release_interpolator
            (attack, delay, release) state event

-- | Monophonic:
--
-- - If the note falls on a new string, release the previously playing note
-- (bend down to its open position) and emit it.
--
-- - If the note falls on the string in use, bend that string up to the note
-- to be played and emit it.
process :: Call.Pitch.Interpolator -> Call.Pitch.Interpolator
    -> (RealTime, RealTime, RealTime) -> State -> Score.Event
    -> Derive.Deriver (State, [Score.Event])
process attack_interpolator release_interpolator
        (attack_time, delay_time, release_time)
        state@(State strings sounding_string prev) event = do
    pitch <- Derive.require "no pitch" $ Score.initial_pitch event
    nn <- Derive.require_right pretty $ PitchSignal.pitch_nn pitch
    case find_string nn strings of
        Nothing -> do
            Log.warn $ pretty nn ++ " below lowest string: " ++ pretty strings
            return (state, [])
        Just string -> do
            new_event <- Derive.require "missing pitches" (emit pitch string)
            return (State strings string event, [new_event])
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
attack :: Call.Pitch.Interpolator -> RealTime -> PitchSignal.Pitch -> RealTime
    -> Score.Event -> Maybe Score.Event
attack interpolator time pitch next_event event = do
    let start_x = next_event - time
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolator start_x start_pitch next_event
        pitch event

-- | After releasing a note, you release your hand, which means the pitch
-- should bend down to the open string.
release :: Call.Pitch.Interpolator -> RealTime -> RealTime
    -> PitchSignal.Pitch -> RealTime -> Score.Event
    -> Maybe Score.Event
release interpolator delay time pitch next_event event = do
    let start_x = next_event + delay
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolator start_x start_pitch
        (start_x + time) pitch event

merge_curve :: Call.Pitch.Interpolator -> RealTime -> PitchSignal.Pitch
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
    -- | The strings are tuned to Pitches, but to compare Pitches I have to
    -- evaluate them to NoteNumbers first.
    --
    -- Since the strings are NoteNumbers, it means they can't retune based on
    -- context as 'PitchSignal.Pitch's can.  I can think about supporting this
    -- later if I need it.
    state_strings :: !(Map.Map Pitch.NoteNumber PitchSignal.Pitch)
    -- | The string that is currently sounding.  This is the /open/ pitch.
    , state_sounding :: !(Pitch.NoteNumber, PitchSignal.Pitch)
    -- | The previous event.  Since each event is modified based on the next
    -- pitch, each 'process' call is one event ahead of the actual event
    -- emitted.
    , state_event :: !Score.Event
    } deriving (Show)

initial_state :: [(Pitch.NoteNumber, PitchSignal.Pitch)] -> Score.Event
    -> Maybe State
initial_state open_strings event = do
    nn <- Score.initial_nn event
    string <- find_string nn strings
    return $ State
        { state_strings = strings
        , state_sounding = string
        , state_event = event
        }
    where
    strings = Map.fromList open_strings
