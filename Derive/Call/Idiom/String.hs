-- | Post-processing to apply a string idiom.
module Derive.Call.Idiom.String where
import qualified Data.Set as Set
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Set as Set

import Ui

import qualified Derive.Call as Call
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.CallSig as CallSig
import Derive.CallSig (control, optional)
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("string-guzheng", c_guzheng pentatonic_c)
    , ("string-viola", c_violin [Twelve.c4, Twelve.g4, Twelve.d5, Twelve.a5])
    ]

-- | This is just a placeholder for now.  Any real use will want to put
-- the appropriate degrees from the appropriate scale in here, either via
-- the static config or some kind of tracklang list literal.
pentatonic_c :: [Pitch.Degree]
pentatonic_c = map Pitch.Degree $
    takeWhile (<=127) [oct*12 + int | oct <- [0..], int <- intervals]
    where intervals = [0, 2, 4, 7, 9]

-- | A string idiom in the style of a 古箏 or other zither where strings must
-- be manually bent to tune them.
c_guzheng :: [Pitch.Degree] -> Derive.NoteCall
c_guzheng strings = Derive.transformer "guzheng" $ \args deriver ->
    CallSig.call3 args
    ( optional "attack" (control "guzheng-attack" 0.5)
    , optional "release" (control "guzheng-release" 0.5)
    , optional "delay" (control "guzheng-delay" 0)) $
    \attack release delay -> do
        srate <- Call.get_srate
        events <- deriver
        let linear = Call.Pitch.interpolator srate id
        output <- string_idiom linear linear strings
            attack delay release events
        Call.cue output

-- | A string idiom in the style of stopped strings like the violin family.
-- Strings instantly jump to their pitches.
c_violin :: [Pitch.Degree] -> Derive.NoteCall
c_violin strings = Derive.transformer "violin" $ \args deriver ->
    CallSig.call1 args
    (optional "delay" (control "string-delay" 0)) $
    \delay -> do
        srate <- Call.get_srate
        events <- deriver
        let linear = Call.Pitch.interpolator srate id
            attack = TrackLang.ConstantControl 0
            release = TrackLang.ConstantControl 0
        output <- string_idiom linear linear strings
            attack delay release events
        Call.cue output


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
-- TODO It would be possible to have a polyphonic effect by allowing more than
-- one stopped string at a time.
string_idiom ::
    Call.PitchInterpolator -- ^ interpolator to draw the attack curve
    -> Call.PitchInterpolator -- ^ draw the release curve
    -> [Pitch.Degree] -- ^ Pitches of open strings.
    -> TrackLang.Control -- ^ Attack time.
    -> TrackLang.Control -- ^ Release delay.
    -> TrackLang.Control -- ^ Time for string to return to its open pitch.
    -> [Score.Event] -> Derive.Deriver [Score.Event]
string_idiom attack_interpolator release_interpolator
        open_strings attack delay release events =
    case events of
        [] -> return []
        event : rest -> case initial_state open_strings event of
            Nothing -> Derive.throw $ "initial degree below lowest string: "
                ++ show (Score.initial_pitch event)
            Just state -> do
                (final, result) <- Call.map_signals [attack, delay, release] []
                    (\[attack, delay, release] [] ->
                        process attack_interpolator release_interpolator
                            (attack, delay, release))
                    state rest
                return $ Derive.merge_asc_events result ++ [state_event final]

-- | Monophonic:
-- If the note falls on a new string, release the previously playing note (bend
-- down to its open position) and emit it.
-- If the note falls on the string in use, bend that string up to the note to
-- be played and emit it.
process :: Call.PitchInterpolator -> Call.PitchInterpolator
    -> (Signal.Y, Signal.Y, Signal.Y) -> State -> Score.Event
    -> Derive.Deriver (State, [Score.Event])
process attack_interpolator release_interpolator
        (attack_time, delay_time, release_time)
        state@(State strings sounding_string prev)
        event =
    case find_string degree strings of
        Nothing -> do
            Log.warn $ "event at " ++ Pretty.pretty start
                ++ " below lowest string: " ++ show degree
            return (state, [])
        Just string -> return (State strings string event, [emit string])
    where
    start = Score.event_start event
    emit string
        | string == sounding_string = attack attack_interpolator
            (RealTime attack_time) degree start prev
        | otherwise = release release_interpolator
            (RealTime delay_time) (RealTime release_time) sounding_string
            start prev
    degree = Score.initial_pitch event

-- | Bend the event up to the next note.
attack :: Call.PitchInterpolator -> RealTime -> Pitch.Degree -> RealTime
    -> Score.Event -> Score.Event
attack interpolator time degree next_event event =
    merge_curve interpolator start_x start_y next_event degree event
    where
    start_x = next_event - time
    start_y = Score.degree_at start_x event

-- | Bend the event down to the given degree.
release :: Call.PitchInterpolator -> RealTime -> RealTime -> Pitch.Degree
    -> RealTime -> Score.Event -> Score.Event
release interpolator delay time degree next_event event =
    merge_curve interpolator start_x start_y (start_x + time) degree event
    where
    start_x = next_event + delay
    start_y = Score.degree_at start_x event

merge_curve :: Call.PitchInterpolator -> RealTime -> Pitch.Degree
    -> RealTime -> Pitch.Degree -> Score.Event -> Score.Event
merge_curve interpolator x0 y0 x1 y1 event
    | y0 == y1 = event
    | otherwise = event { Score.event_pitch = new_pitch }
    where
    scale_id = PitchSignal.sig_scale (Score.event_pitch event)
    curve = interpolator scale_id False x0 y0 x1 y1
    new_pitch = PitchSignal.merge [Score.event_pitch event, curve]

find_string :: Pitch.Degree -> Set.Set Pitch.Degree -> Maybe Pitch.Degree
find_string = Set.lookup_below

data State = State {
    state_strings :: Set.Set Pitch.Degree
    -- | The string that is currently sounding.  This is the /open/ pitch.
    , state_sounding :: Pitch.Degree
    , state_event :: Score.Event
    } deriving (Show)

initial_state :: [Pitch.Degree] -> Score.Event -> Maybe State
initial_state open_strings event = case find_string degree strings of
        Nothing -> Nothing
        Just string -> Just $ State strings string event
    where
    strings = Set.fromList open_strings
    degree = Score.initial_pitch event
