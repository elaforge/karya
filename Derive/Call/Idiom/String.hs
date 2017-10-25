-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-processing to apply a string idiom.
module Derive.Call.Idiom.String where
import qualified Data.Map as Map

import qualified Util.Map
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (control)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("bent-string", c_bent_string)
    , ("stopped-string", c_stopped_string)
    , ("mute-end", c_mute_end)
    ]

module_ :: Module.Module
module_ = "idiom" <> "string"

c_bent_string :: Derive.Transformer Derive.Note
c_bent_string = Derive.transformer module_ "bent-string"
    (Tags.postproc <> Tags.inst)
    ("Post-process events to play in a stopped string idiom, where\
    \ strings must be bent or stopped to reach non-open pitches.\
    \ Originally it was meant to play in the style of a 古箏 or\
    \ other zither, but may also be appropriate for stopped strings\
    \ like the violin family. As a side-effect, events get a"
    <> ShowVal.doc EnvKey.string <> " variable.\
    \\nFurther documentation is in 'Derive.Call.Idiom.String'.")
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
    ) $ \(attack, release, release_delay, open_strings) _args deriver ->
    Ly.when_lilypond deriver $ do
        attack <- Typecheck.to_typed_function attack
        release <- Typecheck.to_typed_function release
        release_delay <- Typecheck.to_typed_function release_delay
        config <- make_config attack release_delay release open_strings
        string_idiom config =<< deriver

c_stopped_string :: Derive.Transformer Derive.Note
c_stopped_string = Derive.transformer module_ "stopped-string"
    (Tags.postproc <> Tags.inst)
    "A specialization of `bent-string` but for stopped strings, like the\
    \ violin family, where strings instantly jump to their pitches."
    $ Sig.callt ((,)
    <$> Sig.defaulted "delay" (control "string-delay" 0)
        "String release delay time."
    <*> open_strings_env
    ) $ \(release_delay, open_strings) _args deriver -> do
        release_delay <- Typecheck.to_typed_function release_delay
        config <- make_config
            (const (Score.untyped 0)) release_delay (const (Score.untyped 0))
            open_strings
        string_idiom config =<< deriver

open_strings_env :: Sig.Parser [PSignal.Pitch]
open_strings_env = Sig.check non_empty $
    Sig.environ_key EnvKey.open_strings [] "Pitches of the open strings."
    where
    non_empty xs
        | null xs = Just "open-strings required"
        | otherwise = Nothing

data Config = Config {
    _open_strings :: Map Pitch.NoteNumber PSignal.Pitch
    -- | (curve, time)
    , _attack_curve :: (PitchUtil.Interpolate, Typecheck.TypedFunction)
    , _release_curve :: (PitchUtil.Interpolate, Typecheck.TypedFunction)
    , _release_delay :: Typecheck.TypedFunction
    }

make_config :: Typecheck.TypedFunction -> Typecheck.TypedFunction
    -> Typecheck.TypedFunction -> [PSignal.Pitch] -> Derive.Deriver Config
make_config attack_dur release_delay release_dur open_strings = do
    srate <- Call.get_srate
    let linear = PitchUtil.interpolate_segment srate id
    -- Coerce is ok because I don't want open strings in the environ to
    -- transpose.
    open_nns <- mapM (Pitches.pitch_nn . PSignal.coerce) open_strings
    return $ Config
        { _open_strings = Map.fromList (zip open_nns open_strings)
        , _attack_curve = (linear, attack_dur)
        , _release_curve = (linear, release_dur)
        , _release_delay = release_delay
        }

{- | Post-process events to play them in a string-like idiom.

    This tweaks the ends of the pitch signals of notes.  When a new note is
    played, the next event is examined to determine if it will share a string
    or not.

    If the string must be used for the following note, the end of the event is
    bent up to the next pitch before the next event is triggered.  This is
    called the \"attack\".  A fast attack gives the sound of a stopped string,
    a slow one sounds like a bent one.

    If the string won't be used for a long enough time, it will be released
    after a delay.  The release time determines how long it will take to reach
    its open pitch.  Since the release happens after the note ends, only
    instruments with a bit of decay will have an audible release.

    This does't do anything fancy like simulate hand position or alternate
    fingerings.  It just selects the lowest string below or at the lowest pitch
    in the note.

    TODO option to override string selection
-}
string_idiom :: Config -> Stream.Stream Score.Event -> Derive.NoteDeriver
string_idiom config = do
    Post.emap1m_ (fst . fst) event1 . Post.next_by (fst . snd)
        <=< Post.emap1m_ id assign
    where
    assign event = do
        let (nns, _warns) = Score.nn_signal event
        lowest <- Derive.require "no pitch" $
            Pitch.nn . snd <$> Signal.minimum nns
        string <- Derive.require ("below lowest string: " <> pretty lowest) $
            Util.Map.lookup_below lowest (_open_strings config)
        return
            ( Score.modify_environ (Env.insert_val EnvKey.string (fst string))
                event
            , string
            )
    event1 ((event, string), next) = process config event string (fst <$> next)

-- | The next note has the same string, so bend this one to prepare.
-- If it's over a certain threshold away, release this one.
-- TODO maybe skip release and attack if the next note is over a certain
-- threshold away?  If its > the decay time, pitch curves would be inaudible...
-- unless of course changing the pitch itself causes a sound, as it might for
-- a fret slide.
process :: Config -> Score.Event -> (Pitch.NoteNumber, PSignal.Pitch)
    -> Maybe Score.Event -> Derive.Deriver Score.Event
process config event string Nothing =
    add_release config (snd string) Nothing event
process config event string (Just next_event) = do
    let next = Score.event_start next_event
    attack_dur <- Call.real_duration_at (snd (_attack_curve config)) next
    event <- add_release config (snd string) (Just (next - attack_dur)) event
    return $ add_attack config attack_dur next_event event

-- | After releasing a note, you release your hand, which means the pitch
-- should bend down to the open string.
add_release :: Config -> PSignal.Pitch -> Maybe RealTime -> Score.Event
    -> Derive.Deriver Score.Event
add_release config open_string maybe_next event = do
    let end = Score.event_end event
    delay <- Call.real_duration_at (_release_delay config) end
    dur <- Call.real_duration_at (snd (_release_curve config)) end
    -- Release if there's enough time for both delay, release curve, and
    -- following attack curve.
    let enough_time = case maybe_next of
            Nothing -> True
            Just next -> next - end > delay + dur
    return $ fromMaybe event $ if not enough_time
        then Nothing else do
            from_pitch <- Score.pitch_at (end+delay) event
            return $ merge_curve (fst (_release_curve config))
                (end + delay) from_pitch
                (end + delay + dur) open_string
                event

-- | Bend the event up to the next note.
--
-- If there isn't enough time, do the bend faster.
-- TODO It might be nice to make the transition spill over into the next
-- attack.
add_attack :: Config -> RealTime -> Score.Event -> Score.Event -> Score.Event
add_attack config dur next_event event
    | dur <= 0 = event
    | otherwise = fromMaybe event $ do
        let start_x = max (Score.event_start event) (next - dur)
        start_pitch <- Score.pitch_at start_x event
        next_pitch <- Score.pitch_at next next_event
        return $ merge_curve (fst (_attack_curve config)) start_x start_pitch
            next next_pitch event
    where next = Score.event_start next_event

merge_curve :: PitchUtil.Interpolate -> RealTime -> PSignal.Pitch
    -> RealTime -> PSignal.Pitch -> Score.Event -> Score.Event
merge_curve interpolate x0 y0 x1 y1 event = Score.set_pitch new_pitch event
    where
    curve = interpolate True x0 y0 x1 y1
    new_pitch = PSignal.append (Score.event_transformed_pitch event) curve


-- * mute end

c_mute_end :: Derive.Transformer Derive.Note
c_mute_end = Derive.transformer module_ "mute-end"
    (Tags.postproc <> Tags.inst)
    ("Put a +mute note at the end of each note, unless there's another note on\
    \ the same string. The " <> ShowVal.doc ring <> " attr suppresses this.")
    $ Sig.callt (
    Sig.required "threshold" "Mute if the string is free for this long."
    ) $ \threshold _args deriver -> Ly.when_lilypond deriver $
        Post.emap_ (mute_end threshold) . Post.nexts_by string_of <$> deriver
    where
    string_of :: Score.Event -> Maybe Text
    string_of = Env.maybe_val EnvKey.string . Score.event_environ

mute_end :: RealTime -> (Score.Event, [Score.Event]) -> [Score.Event]
mute_end threshold (event, nexts)
    | should_mute = [event, set_mute end event]
    | otherwise = [event]
    where
    end = Score.event_end event
    should_mute = not (Score.has_attribute ring event) && case nexts of
        [] -> True
        next : _ -> Score.event_start next - end > threshold

ring :: Attrs.Attributes
ring = Attrs.attr "ring"

set_mute :: RealTime -> Score.Event -> Score.Event
set_mute start event = Score.add_attributes Attrs.mute $
    event { Score.event_start = start, Score.event_duration = 0 }
