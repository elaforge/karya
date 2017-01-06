-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-processing to apply a string idiom.
module Derive.Call.Idiom.String where
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Derive.Call as Call
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (control)
import qualified Derive.Stream as Stream
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("bent-string", c_bent_string)
    , ("stopped-string", c_stopped_string)
    ]

module_ :: Module.Module
module_ = "idiom" <> "string"

c_bent_string :: Derive.Transformer Derive.Note
c_bent_string = Derive.transformer module_ "bent-string"
    (Tags.postproc <> Tags.inst)
    "Post-process events to play in a monophonic string-like idiom, where\
    \ strings must be bent or stopped to reach non-open pitches.\
    \ Originally it was meant to play in the style of a 古箏 or\
    \ other zither, but may also be appropriate for stopped strings\
    \ like the violin family. This only makes sense for instruments with some\
    \ decay, since otherwise you can't hear the transitions.\
    \ Further documentation is in 'Derive.Call.Idiom.String'."
    $ Sig.callt ((,,,)
    <$> Sig.defaulted "attack" (control "string-attack" 0.15)
        "Time for a string to bend to its desired pitch. A fast attack\
        \ sounds like a stopped string."
    <*> Sig.defaulted "release" (control "string-release" 0.1)
        "Time for a string to return to its original pitch."
    <*> Sig.defaulted "delay" (control "string-delay" 0)
        "If the string won't be used for the following note, it will be\
        \ released after this delay."
    <*> open_strings_env
    ) $ \(attack, release, delay, open_strings) _args deriver ->
    Lily.when_lilypond deriver $ do
        srate <- Call.get_srate
        let linear = PitchUtil.interpolate_segment srate id
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
        srate <- Call.get_srate
        let attack = BaseTypes.constant_control 0
            release = BaseTypes.constant_control 0
        let linear = PitchUtil.interpolate_segment srate id
        string_idiom linear linear open_strings attack delay release =<< deriver

open_strings_env :: Sig.Parser [PSignal.Pitch]
open_strings_env = Sig.check non_empty $
    Sig.environ (unsym EnvKey.open_strings) Sig.Unprefixed []
        "Pitches of the open strings."
    where
    unsym (BaseTypes.Symbol sym) = Derive.ArgName sym
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
    PitchUtil.Interpolate -- ^ interpolator to draw the attack curve
    -> PitchUtil.Interpolate -- ^ draw the release curve
    -> [PSignal.Pitch] -- ^ Pitches of open strings.
    -> BaseTypes.ControlRef -- ^ Attack time.
    -> BaseTypes.ControlRef -- ^ Release delay.
    -> BaseTypes.ControlRef -- ^ Time for string to return to its open pitch.
    -> Stream.Stream Score.Event -> Derive.NoteDeriver
string_idiom attack_interpolate release_interpolate open_strings attack delay
        release all_events = Post.event_head all_events $ \event events -> do
    -- Coerce is ok because I don't want open strings in the environ to
    -- transpose.
    open_nns <- mapM (Pitches.pitch_nn . PSignal.coerce) open_strings
    let strings = Map.fromList (zip open_nns open_strings)
    case initial_state strings event of
        Nothing -> Derive.throw $ "initial pitch below lowest string: "
            <> pretty (Score.initial_nn event)
        Just state -> do
            attack <- Post.control id attack events
            delay <- Post.control id delay events
            release <- Post.control id release events
            (final, result) <- Post.emap_asc_m snd (one_event strings) state
                (Stream.zip (zip3 attack delay release) events)
            return $! result <> Stream.from_event (state_event final)
    where
    one_event strings state ((attack, delay, release), event) = do
        start <- Derive.score (Score.event_start event)
        let dur = Call.typed_real_duration Typecheck.Real start
        attack <- dur attack
        delay <- dur delay
        release <- dur release
        process strings attack_interpolate release_interpolate
            (attack, delay, release) state event

{- | Monophonic:

    - If the note falls on a new string, release the previously playing note
    (bend down to its open position) and emit it.

    - If the note falls on the string in use, bend that string up to the note
    to be played and emit it.
-}
process :: Map Pitch.NoteNumber PSignal.Pitch
    -- ^ The strings are tuned to Pitches, but to compare Pitches I have to
    -- evaluate them to NoteNumbers first.
    -> PitchUtil.Interpolate -> PitchUtil.Interpolate
    -> (RealTime, RealTime, RealTime) -> State -> Score.Event
    -> Derive.Deriver (State, [Score.Event])
process strings attack_interpolate release_interpolate
        (attack_time, delay_time, release_time)
        state@(State sounding_string prev) event = do
    pitch <- Derive.require "no pitch" $ Score.pitch_at start event
    nn <- Derive.require_right pretty $
        PSignal.pitch_nn $ Score.apply_controls event start pitch
    case find_string nn strings of
        Nothing -> do
            Log.warn $ pretty nn <> " below lowest string: " <> pretty strings
            return (state, [])
        Just string -> do
            new_event <- Derive.require "missing pitches" (emit pitch string)
            return (State string event, [new_event])
    where
    start = Score.event_start event
    emit pitch string
        -- Re-use an already sounding string.
        | fst string == fst sounding_string = attack attack_interpolate
            attack_time pitch start prev
        -- Sounding string is no longer used, so it can be released.
        | otherwise = release release_interpolate delay_time release_time
            (snd sounding_string) start prev

-- | Bend the event up to the next note.
attack :: PitchUtil.Interpolate -> RealTime -> PSignal.Pitch -> RealTime
    -> Score.Event -> Maybe Score.Event
attack interpolate time pitch next_event event = do
    -- If there isn't enough time, do the bend faster.  I think I'd like to
    -- emit part of the transition, and then complete it after the attack, but
    -- that would require some more complicated code and I'm not sure I need
    -- it.
    let start_x = max (Score.event_start event) (next_event - time)
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolate start_x start_pitch next_event
        pitch event

-- | After releasing a note, you release your hand, which means the pitch
-- should bend down to the open string.
release :: PitchUtil.Interpolate -> RealTime -> RealTime
    -> PSignal.Pitch -> RealTime -> Score.Event
    -> Maybe Score.Event
release interpolate delay time pitch next_event event = do
    let start_x = next_event + delay
    start_pitch <- Score.pitch_at start_x event
    return $ merge_curve interpolate start_x start_pitch
        (start_x + time) pitch event

merge_curve :: PitchUtil.Interpolate -> RealTime -> PSignal.Pitch
    -> RealTime -> PSignal.Pitch -> Score.Event -> Score.Event
merge_curve interpolate x0 y0 x1 y1 event =
    Score.set_pitch new_pitch event
    where
    curve = interpolate False x0 y0 x1 y1
    new_pitch = Score.event_transformed_pitch event <> curve

find_string :: Pitch.NoteNumber -> Map Pitch.NoteNumber PSignal.Pitch
    -> Maybe (Pitch.NoteNumber, PSignal.Pitch)
find_string = Map.lookup_below

-- | Keep track of the current string state.
data State = State {
    -- | The string that is currently sounding.  This is the /open/ pitch.
    state_sounding :: !(Pitch.NoteNumber, PSignal.Pitch)
    -- | The previous event.  Since each event is modified based on the next
    -- pitch, each 'process' call is one event ahead of the actual event
    -- emitted.
    , state_event :: !Score.Event
    } deriving (Show)

initial_state :: Map Pitch.NoteNumber PSignal.Pitch -> Score.Event
    -> Maybe State
initial_state strings event = do
    nn <- Score.initial_nn event
    string <- find_string nn strings
    return $ State
        { state_sounding = string
        , state_event = event
        }
