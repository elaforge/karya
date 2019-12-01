-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Playing idioms of plucked or bowed strings.  Generally these calls expect
-- 'EnvKey.open_strings', and possibly 'EnvKey.string'.
module Derive.C.Idiom.String where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Post as Post
import qualified Derive.Call.StringUtil as StringUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import           Derive.Sig (control)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("gliss-a", c_gliss_absolute)
        , ("gliss", c_gliss)
        , ("on", c_nth_harmonic)
        , ("o", c_harmonic)
        ]
    , Library.transformers
        [ ("bent-string", c_bent_string)
        , ("stopped-string", c_stopped_string)
        , ("mute-end", c_mute_end)
        ]
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
    <*> StringUtil.open_strings_env
    ) $ \(attack, release, release_delay, open_strings) _args deriver ->
    Ly.when_lilypond deriver $ do
        attack <- Typecheck.to_typed_function attack
        release <- Typecheck.to_typed_function release
        release_delay <- Typecheck.to_typed_function release_delay
        -- TODO I used to use just the pitch, but the index is more useful when
        -- the pitch can change.  But I have to use pitch for harmonics below
        -- because they don't always require open_strings.
        open_strings <- StringUtil.indexed_strings open_strings
        config <- make_config attack release_delay release open_strings
        string_idiom config =<< deriver

c_stopped_string :: Derive.Transformer Derive.Note
c_stopped_string = Derive.transformer module_ "stopped-string"
    (Tags.postproc <> Tags.inst)
    "A specialization of `bent-string` but for stopped strings, like the\
    \ violin family, where strings instantly jump to their pitches."
    $ Sig.callt ((,,)
    <$> Sig.defaulted "delay" (control "string-delay" 0)
        "String release delay time."
    <*> StringUtil.open_strings_env <*> StringUtil.string_env
    ) $ \(release_delay, open_strings, string) _args deriver -> case string of
        Just _ -> deriver -- presumably they will all get a constant string
        Nothing -> do
            open_strings <- StringUtil.indexed_strings open_strings
            release_delay <- Typecheck.to_typed_function release_delay
            config <- make_config
                (const (ScoreT.untyped 0)) release_delay
                (const (ScoreT.untyped 0)) open_strings
            string_idiom config =<< deriver

data Config = Config {
    _open_strings :: Map Pitch.NoteNumber StringUtil.String
    -- | (curve, time)
    , _attack_curve :: (PitchUtil.Interpolate, Typecheck.TypedFunction)
    , _release_curve :: (PitchUtil.Interpolate, Typecheck.TypedFunction)
    , _release_delay :: Typecheck.TypedFunction
    }

make_config :: Typecheck.TypedFunction -> Typecheck.TypedFunction
    -> Typecheck.TypedFunction -> [StringUtil.String] -> Derive.Deriver Config
make_config attack_dur release_delay release_dur open_strings = do
    srate <- Call.get_srate
    let linear = PitchUtil.segment srate ControlUtil.Linear
    return $ Config
        { _open_strings =
            Map.fromList $ Seq.key_on StringUtil.str_nn open_strings
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
-}
string_idiom :: Config -> Stream.Stream Score.Event -> Derive.NoteDeriver
string_idiom config = do
    Post.emap1m_ (fst . fst) event1 . Post.next_by (StringUtil.str_nn . snd)
        <=< Post.emap1m_ id assign
    where
    assign event = do
        let (nns, _warns) = Score.nn_signal event
        when (Signal.null nns) $ Derive.throw "no pitch"
        let lowest = Pitch.nn $ Signal.minimum nns
        string <- Derive.require ("below lowest string: " <> pretty lowest) $
            snd <$> Map.lookupLE lowest (_open_strings config)
        return
            ( Score.modify_environ (StringUtil.insert_string string) event
            , string
            )
    event1 ((event, string), next) = process config event string (fst <$> next)

-- | The next note has the same string, so bend this one to prepare.
-- If it's over a certain threshold away, release this one.
-- TODO maybe skip release and attack if the next note is over a certain
-- threshold away?  If its > the decay time, pitch curves would be inaudible...
-- unless of course changing the pitch itself causes a sound, as it might for
-- a fret slide.
process :: Config -> Score.Event -> StringUtil.String
    -> Maybe Score.Event -> Derive.Deriver Score.Event
process config event string Nothing =
    add_release config (StringUtil.str_pitch string) Nothing event
process config event string (Just next_event) = do
    let next = Score.event_start next_event
    attack_dur <- Call.real_duration_at (snd (_attack_curve config)) next
    event <- add_release config (StringUtil.str_pitch string)
        (Just (next - attack_dur)) event
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
merge_curve interpolate x0 y0 x1 y1 event =
    Score.set_pitch (Score.event_pitch event <> curve) event
    where curve = interpolate x0 y0 x1 y1


-- * mute end

c_mute_end :: Derive.Transformer Derive.Note
c_mute_end = Derive.transformer module_ "mute-end"
    (Tags.postproc <> Tags.inst)
    ("Put a note and then +mute note at the end of each note, unless there's\
    \ another note on the same string. The " <> ShowVal.doc ring
    <> " attr suppresses this.")
    $ Sig.callt ((,,)
    <$> Sig.required "threshold" "Mute if the string is free for this long."
    <*> Sig.required "dur" "Duration of mute note. If it's zero, its omitted\
        \ and only the +mute note is emitted."
    <*> Sig.required "dyn" "Dyn of mute note."
    ) $ \(threshold, dur, dyn) _args deriver -> Ly.when_lilypond deriver $ do
        dur <- Typecheck.to_function dur
        dyn <- Typecheck.to_function dyn
        Post.emap_ (mute_end (RealTime.seconds . dur) dyn threshold)
            . Post.nexts_by string_of <$> deriver
    where
    string_of :: Score.Event -> Maybe Text
    string_of = Env.maybe_val EnvKey.string . Score.event_environ

mute_end :: (RealTime -> RealTime) -> Typecheck.Function -> RealTime
    -> (Score.Event, [Score.Event]) -> [Score.Event]
mute_end dur_at dyn_at threshold (event, nexts)
    | should_mute = event
        : [mute_note end dur (dyn_at end) event | dur > 0]
        ++ [set_mute (end + dur) event]
    | otherwise = [event]
    where
    dur = dur_at end
    end = Score.event_end event
    should_mute = not (Score.has_attribute ring event) && case nexts of
        [] -> True
        next : _ -> Score.event_start next - end > threshold

ring :: Attrs.Attributes
ring = Attrs.attr "ring"

mute_note :: RealTime -> RealTime -> Signal.Y -> Score.Event -> Score.Event
mute_note start dur dyn event = Score.set_dynamic dyn $ event
    { Score.event_start = start
    , Score.event_duration = dur
    }

set_mute :: RealTime -> Score.Event -> Score.Event
set_mute start event = Score.add_attributes Attrs.mute $
    event { Score.event_start = start, Score.event_duration = 0 }


-- * gliss

c_gliss_absolute, c_gliss :: Derive.Generator Derive.Note
c_gliss_absolute = make_gliss "gliss-a" True
c_gliss = make_gliss "gliss" False

-- | This provides two kinds of glissando: absolute takes an absolute amount
-- of time for the entire glissando, while non-absolute takes a certain amount
-- of time per string, and hence will have a different duration depending on
-- how many strings.
make_gliss :: Derive.CallName -> Bool -> Derive.Generator Derive.Note
make_gliss name is_absolute = Derive.generator module_ name mempty
    "Glissando along the open strings. The standard version divides the `time`\
    \ among the number of notes, while the -a (absolute) version gives `time`\
    \ to each note."
    $ Sig.call ((,,,)
    <$> Sig.required "start"
        "Start this many strings above or below the destination pitch."
    <*> (if is_absolute
        then Sig.defaulted "time" (Typecheck.real 0.25)
            "Time in which to play the glissando."
        else Sig.defaulted "time" (Typecheck.real 0.075)
            "Time between each note.")
    <*> Sig.defaulted "dyn" Nothing "Start at this dyn, and interpolate\
        \ to the destination dyn. If not given, the dyn is constant."
    <*> StringUtil.open_strings_env
    ) $ \(gliss_start, time, maybe_start_dyn, open_strings) ->
    Sub.inverting $ \args -> do
        end <- Args.real_start args
        time <- Call.real_duration end time
        dest_pitch <- Call.get_transposed end
        dest_dyn <- Call.dynamic end
        let start_dyn = fromMaybe dest_dyn maybe_start_dyn
        pitches <- gliss_pitches open_strings dest_pitch gliss_start
        let total_time = if is_absolute then time
                else time * fromIntegral (length pitches)
        Ly.when_lilypond (GraceUtil.lily_grace args (end - time) pitches) $
            gliss pitches total_time start_dyn dest_dyn end
                <> Call.placed_note args

gliss_pitches :: [PSignal.Pitch] -> PSignal.Transposed -> Int
    -> Derive.Deriver [PSignal.Pitch]
gliss_pitches open_strings dest_pitch gliss_start = do
    -- Round the NN so a slightly off note start doesn't make this string get
    -- doubled.
    dest_nn <- Num.roundDigits 2 <$> Pitches.pitch_nn dest_pitch
    -- TODO shouldn't need to eval them all
    open_nns <- mapM (Pitches.pitch_nn . PSignal.coerce) open_strings
    let strings = Seq.sort_on snd $ zip open_strings open_nns
    -- 0 2 4 6 8 10
    return $ if gliss_start >= 0
        -- 5 -> 6 8 10 -> 10 8 6 5
        then reverse $ take gliss_start $ map fst $
            dropWhile ((<=dest_nn) . snd) strings
        -- 5 -> 0 2 4
        else Seq.rtake (-gliss_start) $ map fst $
            takeWhile ((<dest_nn) . snd) strings

gliss :: [PSignal.Pitch] -> RealTime -> Signal.Y -> Signal.Y -> RealTime
    -> Derive.NoteDeriver
gliss pitches time start_dyn end_dyn end = do
    let dur = time / fromIntegral (length pitches)
        start = end - time
        ts = take (length pitches) (Seq.range_ start dur)
        dyns = map (Num.scale start_dyn end_dyn . RealTime.to_seconds
            . Num.normalize start end) ts
    score_ts <- mapM Derive.score ts
    score_dur <- Call.score_duration end dur
    let note (t, p, dyn) = Derive.place t score_dur $ Call.with_dynamic dyn $
            Call.pitched_note p
    mconcat $ map note $ zip3 score_ts pitches dyns


-- * harmonic

c_nth_harmonic :: Derive.Generator Derive.Note
c_nth_harmonic = Derive.generator module_ "harmonic" Tags.inst
    "Play a specific harmonic on a specific string."
    $ Sig.call ((,,)
    <$> (Typecheck.positive <$> Sig.defaulted "n" 1 "Play this harmonic.")
    <*> finger_arg
    <*> Sig.required_environ_key EnvKey.string "Play on this string."
    ) $ \(harmonic, finger, string) -> Sub.inverting $ \args -> do
        string <- StringUtil.string string
        finger <- Call.control_at finger =<< Args.real_start args
        Call.place args $
            Derive.with_constant_control Controls.finger finger $
            Call.pitched_note $
            Twelve.nn_pitch $ Pitch.modify_hz (*harmonic) $
            StringUtil.str_nn string

c_harmonic :: Derive.Generator Derive.Note
c_harmonic = Derive.generator module_ "harmonic" Tags.inst
    "Play the given pitch as a harmonic, possibly restricted to a string.\
    \ Otherwise, pick the lowest harmonic where the pitch fits."
    $ Sig.call ((,,,)
        <$> finger_arg
        <*> Sig.defaulted "h1" False
            "Ok to pick an open string as the 1st harmonic?"
        <*> StringUtil.string_env
        <*> StringUtil.open_strings_env
    ) $ \(finger, h1_ok, maybe_string, open_strings) -> Sub.inverting $
    \args -> do
        nn <- Pitches.pitch_nn =<< Call.get_transposed =<< Args.real_start args
        open_strings <- mapM StringUtil.string open_strings
        maybe_string <- traverse StringUtil.string maybe_string
        finger <- Call.control_at finger =<< Args.real_start args
        (string, harmonic) <- Derive.require_right id $
            StringUtil.find_harmonic h1_ok highest_harmonic open_strings
                maybe_string nn
        StringUtil.with_string string $ Call.place args $
            Derive.with_constant_control Controls.finger finger $
            Call.pitched_note $
            Twelve.nn_pitch $ touch_interval harmonic (StringUtil.str_nn string)

highest_harmonic :: StringUtil.Harmonic
highest_harmonic = 13

touch_interval :: Int -> Pitch.NoteNumber -> Pitch.NoteNumber
touch_interval harmonic = Pitch.modify_hz (* fromIntegral harmonic)

finger_arg :: Sig.Parser DeriveT.ControlRef
finger_arg = Sig.defaulted "finger" (DeriveT.constant_control 0.035)
    "Weight of the finger touching the string, in newtons."
