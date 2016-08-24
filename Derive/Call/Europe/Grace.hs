-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls that generate grace notes.  These are short sequences of quick notes
-- whose duration is generally independent of the tempo.
module Derive.Call.Europe.Grace where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Lily as Lily
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("g", c_grace)
    , ("g-", c_grace_hold)
    , ("grace", c_basic_grace)
    , ("roll", c_roll)
    , ("`mordent`", c_mordent (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent (Pitch.Diatonic (-1)))
    ]

pitch_calls :: Derive.CallMaps Derive.Pitch
pitch_calls = Derive.generator_call_map
    [ ("g", c_grace_p)
    , ("`mordent`", c_mordent_p (Pitch.Diatonic 1))
    , ("`rmordent`", c_mordent_p (Pitch.Diatonic (-1)))
    ]


-- | It's pretty much arbitrary, but this seems ok.
default_grace_dur :: Typecheck.DefaultReal
default_grace_dur = Typecheck.real (1/12)

-- * standard args

grace_envs :: Sig.Parser (BaseTypes.Duration, Double, BaseTypes.ControlRef)
grace_envs = (,,) <$> grace_dur_env <*> grace_dyn_env <*> grace_place_env

grace_dur_env :: Sig.Parser BaseTypes.Duration
grace_dur_env = Typecheck._real <$>
    Sig.environ "dur" Sig.Both default_grace_dur "Duration of grace notes."

grace_dyn_env :: Sig.Parser Double
grace_dyn_env =
    Typecheck.non_negative <$> Sig.environ "grace-dyn" Sig.Unprefixed
        0.5 "Scale the dyn of the grace notes."

grace_place_env :: Sig.Parser BaseTypes.ControlRef
grace_place_env = Sig.environ "place" Sig.Both
    (Sig.control "place" 0) grace_place_doc

grace_place_doc :: Doc.Doc
grace_place_doc =
    "At 0, grace notes fall before their base note.  At 1, grace notes fall on\
    \ the base note, and the base note is delayed."


-- * note calls

c_mordent :: Pitch.Transpose -> Derive.Generator Derive.Note
c_mordent default_neighbor = Derive.generator Module.europe "mordent"
    Tags.ornament
    "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,)
    <$> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_envs
    ) $ \(Typecheck.DefaultDiatonic neighbor, (grace_dur, dyn, place)) ->
    Sub.inverting $ \args ->
        Lily.when_lilypond (lily_mordent args neighbor) $ do
            pitch <- Call.get_pitch =<< Args.real_start args
            legato_grace args dyn [pitch, Pitches.transpose neighbor pitch]
                grace_dur place

lily_mordent :: Derive.PassedArgs d -> Pitch.Transpose -> Derive.NoteDeriver
lily_mordent args neighbor = do
    start <- Args.real_start args
    pitch <- Call.get_pitch start
    lily_grace args start [pitch, Pitches.transpose neighbor pitch]

-- | Grace is in the prelude since it's so commonly used.  Mordent and the
-- other variations are still in 'Module.europe'.
c_grace :: Derive.Generator Derive.Note
c_grace = make_grace Module.prelude
    "Emit grace notes. The grace notes go through the `(` call, so they will\
    \ overlap or apply a keyswitch, or do whatever `(` does."
    id $ \args events -> Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call (Args.context args) "(" [] [events]

c_grace_hold :: Derive.Generator Derive.Note
c_grace_hold = make_grace Module.prelude
    "Like `g`, but doesn't use `(`, and all notes are held to the duration of\
    \ the event."
    id $ \_args -> Sub.derive . hold
    where
    hold events = maybe events (\e -> map (set_end e) events) end
        where end = Seq.maximum $ map Sub.event_end events
    set_end end event =
        event { Sub.event_duration = end - Sub.event_start event }

-- | Make a grace call with the standard arguments.
make_grace :: Module.Module -> Doc.Doc
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (Derive.PassedArgs Score.Event -> [Sub.Event] -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
make_grace module_ doc transform derive =
    Derive.generator module_ "grace" (Tags.ornament <> Tags.ly) doc
    $ Sig.call ((,)
    <$> grace_pitches_arg <*> grace_envs
    ) $ \(pitches, (grace_dur, dyn, place)) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args start pitches) $ do
            with_dyn <- (*dyn) <$> (Call.dynamic =<< Args.real_start args)
            derive args =<< basic_grace args pitches
                (transform . Call.with_dynamic with_dyn) grace_dur place

c_basic_grace :: Derive.Generator Derive.Note
c_basic_grace = Derive.generator Module.prelude "basic-grace"
    (Tags.ornament <> Tags.ly)
    "This a grace call where all arguments are required. The idea is that this\
    \ will be used as the implementation of more specific ornaments, perhaps\
    \ defined in a definitions file."
    $ Sig.call ((,,,)
    <$> Sig.required_env "pitches" Sig.None grace_pitches_doc
    <*> Sig.required_env "dur" Sig.None "Duration of grace notes."
    <*> Sig.required_env "place" Sig.None grace_place_doc
    <*> Sig.defaulted_env "transformer" Sig.None Nothing
        "Apply a transformer to grace notes."
    ) $ \(pitches, grace_dur, place, maybe_transform) ->
    Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        let apply = Eval.eval_quoted_transformers (Args.context args)
        Lily.when_lilypond (lily_grace args start pitches) $
            Sub.derive =<< basic_grace args pitches
                (maybe id apply maybe_transform) grace_dur place

lily_grace :: Derive.PassedArgs d -> RealTime -> [PSignal.Pitch]
    -> Derive.NoteDeriver
lily_grace args start pitches = do
    pitches <- mapM Lily.pitch_to_lily =<< mapM (Derive.resolve_pitch start)
        pitches
    let ly_notes = map (<> Lilypond.to_lily Lilypond.D8) pitches
        beamed = Seq.first_last (<>"[") (<>"]") ly_notes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> Text.unwords beamed <> " } "
    -- Prepending to the note instead of emitting a separate Lily.code ensures
    -- it stays with the note's voice.
    Lily.prepend_code code $ Call.place args Call.note

legato_grace :: Derive.NoteArgs -> Signal.Y -> [PSignal.Pitch]
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.NoteDeriver
legato_grace args dyn_scale pitches grace_dur place = do
    dyn <- (*dyn_scale) <$> (Call.dynamic =<< Args.real_start args)
    events <- basic_grace args pitches (Call.with_dynamic dyn) grace_dur place
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call (Args.context args) "(" [] [events]

basic_grace_dyn :: Signal.Y -> Derive.PassedArgs a -> [PSignal.Pitch]
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.NoteDeriver
basic_grace_dyn dyn_scale args pitches grace_dur place = do
    dyn <- (*dyn_scale) <$> (Call.dynamic =<< Args.real_start args)
    Sub.derive
        =<< basic_grace args pitches (Call.with_dynamic dyn) grace_dur place

basic_grace :: Derive.PassedArgs a -> [PSignal.Pitch]
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.Deriver [Sub.Event]
basic_grace args pitches transform =
    make_grace_notes (Args.prev_start args) (Args.range_or_next args) notes
    where notes = map (transform . Call.pitched_note) pitches ++ [Call.note]

make_grace_notes :: Maybe ScoreTime -> (ScoreTime, ScoreTime) -- ^ (start, end)
    -> [Derive.NoteDeriver] -> BaseTypes.Duration -> BaseTypes.ControlRef
    -- ^ grace placement, 'grace_place_doc'
    -> Derive.Deriver [Sub.Event]
make_grace_notes prev (start, end) notes grace_dur place = do
    real_start <- Derive.real start
    place <- Num.clamp 0 1 <$> Call.control_at place real_start
    case grace_dur of
        BaseTypes.ScoreDuration grace_dur -> do
            let extents = fit_grace_durs (ScoreTime.double place)
                    prev start end (length notes) grace_dur
            return [Sub.Event s d n | ((s, d), n) <- zip extents notes]
        BaseTypes.RealDuration grace_dur -> do
            real_end <- Derive.real end
            real_prev <- maybe (return Nothing) ((Just <$>) . Derive.real) prev
            let extents = fit_grace_durs (RealTime.seconds place)
                    real_prev real_start real_end (length notes) grace_dur
            zipWithM note_real extents notes
    where
    note_real (start, dur) note = do
        score_start <- Derive.score start
        score_end <- Derive.score (start + dur)
        return $ Sub.Event score_start (score_end - score_start) note

c_attr_grace :: Map.Map Int Attrs.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
c_attr_grace supported =
    Derive.generator Module.europe "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> TextUtil.join ", " (map ShowVal.doc (Map.elems supported))
    ) $ Sig.call ((,,)
    <$> grace_pitches_arg <*> grace_envs
    <*> Sig.environ "attr" Sig.Prefixed Nothing
        "If given, put this attr on the grace notes. Otherwise, pick a grace\
        \ note from the support list."
    ) $ \(pitches, (grace_dur, dyn, place), attr) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        Lily.when_lilypond (lily_grace args start pitches) $ case attr of
            Just attr -> Call.add_attributes attr $
                basic_grace_dyn dyn args pitches grace_dur place
            Nothing -> do
                maybe_attrs <- grace_attributes start supported pitches base
                case maybe_attrs of
                    Just attrs -> attr_grace start args grace_dur
                        (length pitches) attrs
                    -- Fall back on normal grace.
                    Nothing -> legato_grace args dyn pitches grace_dur place
    where
    attr_grace real_start args grace_dur notes attrs = do
        let (start, dur) = Args.extent args
        grace_dur <- Call.score_duration start grace_dur
        dyn <- Call.dynamic real_start
        let before = fromIntegral notes * grace_dur
        pitch <- Call.get_pitch real_start
        Derive.place (start - before) (dur + before) $
            Call.add_attributes attrs $ Call.with_dynamic dyn $
            Call.pitched_note pitch

grace_attributes :: RealTime -> Map.Map Int Attrs.Attributes -> [PSignal.Pitch]
    -> PSignal.Pitch -> Derive.Deriver (Maybe Attrs.Attributes)
grace_attributes pos supported [grace] base = do
    base <- Derive.resolve_pitch pos base
    grace <- Derive.resolve_pitch pos grace
    diff <- (-) <$> Pitches.pitch_nn base <*> Pitches.pitch_nn grace
    return $ Map.lookup (round diff) supported
grace_attributes _ _ _ _ = return Nothing

c_roll :: Derive.Generator Derive.Note
c_roll = Derive.generator Module.europe "roll" Tags.ornament
    "These are like grace notes, but they all have the same pitch.\
    \ The extra notes always fall before the main one, because `trem` covers\
    \ the afterwards case."
    $ Sig.call ((,,)
    <$> Sig.defaulted "times" 1 "Number of grace notes."
    <*> Sig.defaulted "time" default_grace_dur "Time between the strokes."
    <*> Sig.defaulted "dyn" 0.5 "Dyn scale for the grace notes."
    ) $ \(times, Typecheck.DefaultReal time, dyn_scale) ->
    Sub.inverting $ roll (times+1) time dyn_scale

roll :: Int -> BaseTypes.Duration -> Signal.Y -> Derive.PassedArgs a
    -> Derive.NoteDeriver
roll times time dyn_scale args = do
    start <- Args.real_start args
    pitch <- Call.get_pitch start
    dyn <- Call.dynamic start
    notes <- repeat_notes (Call.with_pitch pitch Call.note) times time 0 args
    Sub.derive $ case Seq.viewr notes of
        Just (graces, main) ->
            map (fmap (Call.with_dynamic (dyn*dyn_scale))) graces ++ [main]
        Nothing -> []

repeat_notes :: Derive.NoteDeriver -> Int -> BaseTypes.Duration
    -> Signal.Y -- ^ placement, 'grace_place_doc'
    -> Derive.PassedArgs a -> Derive.Deriver [Sub.Event]
repeat_notes note times time place args =
    make_grace_notes (Args.prev_start args)
        (Args.range_or_next args) (replicate times note) time
        (BaseTypes.constant_control place)

-- * pitch calls

c_mordent_p :: Pitch.Transpose -> Derive.Generator Derive.Pitch
c_mordent_p default_neighbor = Derive.generator1 Module.europe "mordent"
    Tags.ornament "Like `g`, but hardcoded to play pitch, neighbor, pitch."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> Sig.defaulted "neighbor" (Typecheck.DefaultDiatonic default_neighbor)
        "Neighbor pitch."
    <*> grace_dur_env
    ) $ \(pitch, Typecheck.DefaultDiatonic neighbor, grace_dur) args ->
        grace_p grace_dur [pitch, Pitches.transpose neighbor pitch, pitch]
            (Args.range_or_next args)

c_grace_p :: Derive.Generator Derive.Pitch
c_grace_p = Derive.generator1 Module.europe "grace" Tags.ornament
    "Generate grace note pitches.  They start on the event and have the given\
    \ duration, but are shortened if the available duration is too short.\
    \ The destination pitch is first, even though it plays last, so\
    \ `g (c) (a) (b)` produces `a b c`."
    $ Sig.call ((,,)
    <$> Sig.required "pitch" "Base pitch."
    <*> grace_pitches_arg <*> grace_dur_env
    ) $ \(pitch, pitches, grace_dur) args -> do
        ps <- (++[pitch]) <$> resolve_pitches pitch pitches
        grace_p grace_dur ps (Args.range_or_next args)

grace_p :: BaseTypes.Duration -> [PSignal.Pitch]
    -> (ScoreTime, ScoreTime) -> Derive.Deriver PSignal.PSignal
grace_p grace_dur pitches (start, end) = do
    real_dur <- Call.real_duration start grace_dur
    real_start <- Derive.real start
    real_end <- Derive.real end
    let starts = fit_after real_start real_end (length pitches) real_dur
    return $ PSignal.signal $ zip starts pitches

-- * util

grace_pitches_arg :: Sig.Parser [Either PSignal.Pitch Score.TypedVal]
grace_pitches_arg = Sig.many "pitch" grace_pitches_doc

grace_pitches_doc :: Doc.Doc
grace_pitches_doc = "Grace note pitches. If they are numbers,\
    \ they are taken as transpositions and must all be the same type,\
    \ defaulting to diatonic."

resolve_pitches :: PSignal.Pitch -> [Either PSignal.Pitch Score.TypedVal]
    -> Derive.Deriver [PSignal.Pitch]
resolve_pitches base = either Derive.throw return . check_pitches base

check_pitches :: PSignal.Pitch -> [Either PSignal.Pitch Score.TypedVal]
    -> Either Text [PSignal.Pitch]
check_pitches base pitches = do
    make <- case types of
        t : ts
            | all (==t) ts -> case t of
                Score.Diatonic -> Right Pitch.Diatonic
                Score.Chromatic -> Right Pitch.Chromatic
                Score.Nn -> Right Pitch.Nn
                _ -> Left $ "expected transpose type, but got " <> pretty t
            | otherwise ->
                Left $ "arguments should all have the same type, got "
                    <> pretty types
        [] -> Right Pitch.Diatonic
    return $ map (either id (resolve make . Score.typed_val)) pitches
    where
    resolve make n = Pitches.transpose (make n) base
    types = snd . List.mapAccumL type_of Score.Diatonic . Either.rights $
        pitches
    type_of deflt n = case Score.type_of n of
        Score.Untyped -> (deflt, deflt)
        t -> (t, t)

-- | Determine grace note starting times and durations if they are to fit in
-- the given time range, shortening them if they don't fit.
fit_grace_durs :: (Show a, Fractional a, Ord a) => a -> Maybe a -> a -> a
    -> Int -> a -> [(a, a)]
fit_grace_durs place prev start end notes dur =
    map add_dur $ Seq.zip_next $ fit_grace place prev start end notes dur
    where
    add_dur (x, Nothing) = (x, end - x)
    add_dur (x, Just next) = (x, next - x)

fit_grace :: (Show a, Fractional a, Ord a) => a
    -- ^ placement, 'grace_place_doc'
    -> Maybe a -> a -> a -> Int -> a -> [a]
fit_grace place maybe_prev start end notes dur
    | place <= 0 = before
    | place >= 1 = after
    | otherwise = zipWith (\x y -> Num.scale x y place) before after
    where
    after = fit_after start end notes dur
    before = fit_before maybe_prev start notes dur

fit_before :: (Fractional a, Ord a) => Maybe a -> a -> Int -> a -> [a]
fit_before maybe_prev start notes dur =
    take notes $ drop 1 $ Seq.range_ (start - notes_t * step) step
    where
    notes_t = fromIntegral notes
    step
        | Just prev <- maybe_prev, start - dur * notes_t < prev =
            (start - prev) / notes_t
        | otherwise = dur

fit_after :: (Fractional a, Ord a) => a -> a -> Int -> a -> [a]
fit_after start end notes dur = take notes $ Seq.range_ start step
    where
    notes_t = fromIntegral notes
    step
        | dur * notes_t >= end - start = (end - start) / notes_t
        | otherwise = dur
