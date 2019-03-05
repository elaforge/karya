-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to emit grace-note like things.
module Derive.Call.GraceUtil where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- * standard args

grace_envs :: Sig.Parser (BaseTypes.Duration, BaseTypes.ControlRef)
grace_envs = (,) <$> grace_dur_env <*> grace_place_env

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

-- | It's pretty much arbitrary, but this seems ok.
default_grace_dur :: Typecheck.DefaultReal
default_grace_dur = Typecheck.real (1/12)

grace_pitches_arg :: Sig.Parser [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
grace_pitches_arg = Sig.many "pitch" grace_pitches_doc

grace_pitches_doc :: Doc.Doc
grace_pitches_doc = "Grace note pitches. If they are numbers,\
    \ they are taken as transpositions and must all be the same type,\
    \ defaulting to diatonic."

-- * calls

-- | Make a grace call with the standard arguments.
make_grace :: Module.Module -> Doc.Doc
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (Derive.PassedArgs Score.Event -> [Sub.Event] -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
make_grace module_ doc transform derive =
    Derive.generator module_ "grace" (Tags.ornament <> Tags.ly) doc
    $ Sig.call ((,,)
    <$> grace_pitches_arg <*> grace_dyn_env <*> grace_envs
    ) $ \(pitches, dyn, (grace_dur, place)) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        Ly.when_lilypond (lily_grace args start pitches) $ do
            with_dyn <- (*dyn) <$> (Call.dynamic =<< Args.real_start args)
            derive args =<< basic_grace_transform args pitches
                (transform . Call.with_dynamic with_dyn) grace_dur place

-- | This is like 'make_grace', but gives you pitches instead of realized
-- events, in case you want to merge them or something.
make_grace_pitch :: Module.Module -> Doc.Doc
    -> (Derive.PassedArgs Score.Event -> [Sub.GenericEvent PSignal.Pitch]
        -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
make_grace_pitch module_ doc derive =
    Derive.generator module_ "grace" (Tags.ornament <> Tags.ly) doc
    $ Sig.call ((,)
    <$> grace_pitches_arg <*> grace_envs
    ) $ \(pitches, (grace_dur, place)) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        Ly.when_lilypond (lily_grace args start pitches) $ do
            here <- Call.get_pitch_here args
            notes <- basic_grace args (pitches ++ [here]) grace_dur place
            derive args notes

repeat_notes :: Derive.NoteDeriver -> Int -> BaseTypes.Duration
    -> Signal.Y -- ^ placement, 'grace_place_doc'
    -> Derive.PassedArgs a -> Derive.Deriver [Sub.Event]
repeat_notes note times time place args =
    make_grace_notes (Args.prev_start args)
        (Args.range_or_next args) (replicate times note) time
        (BaseTypes.constant_control place)

make_grace_notes :: Maybe ScoreTime -> (ScoreTime, ScoreTime) -- ^ (start, end)
    -> [note] -- ^ the last note is the destination
    -> BaseTypes.Duration
    -> BaseTypes.ControlRef -- ^ placement, see 'grace_place_doc'
    -> Derive.Deriver [Sub.GenericEvent note]
make_grace_notes prev (start, end) notes grace_dur place = do
    real_start <- Derive.real start
    place <- Num.clamp 0 1 <$> Call.control_at place real_start
    case grace_dur of
        BaseTypes.ScoreDuration grace_dur -> do
            let extents = fit_grace_durs (ScoreTime.from_double place)
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

lily_grace :: Derive.PassedArgs d -> RealTime -> [PSignal.Pitch]
    -> Derive.NoteDeriver
lily_grace args start pitches = do
    env <- Derive.get_environ
    pitches <- mapM (Ly.pitch_to_lily env) =<< mapM (Derive.resolve_pitch start)
        pitches
    let ly_notes = map (<> Lilypond.to_lily Lilypond.D8) pitches
        beamed = Seq.first_last (<>"[") (<>"]") ly_notes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> Text.unwords beamed <> " } "
    -- Prepending to the note instead of emitting a separate code event ensures
    -- it stays with the note's voice.
    Ly.add_first (Ly.prepend, code) $ Call.place args Call.note

-- * attr grace

c_attr_grace :: Map Int Attrs.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
c_attr_grace supported =
    Derive.generator Module.instrument "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Doc.commas (map ShowVal.doc (Map.elems supported))
    ) $ Sig.call ((,,,)
    <$> grace_pitches_arg <*> grace_dyn_env <*> grace_envs
    <*> Sig.environ "attr" Sig.Prefixed Nothing
        "If given, put this attr on the grace notes. Otherwise, pick a grace\
        \ note from the support list."
    ) $ \(pitches, dyn, (grace_dur, place), attr) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        base <- Call.get_pitch start
        pitches <- resolve_pitches base pitches
        Ly.when_lilypond (lily_grace args start pitches) $ case attr of
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

grace_attributes :: RealTime -> Map Int Attrs.Attributes -> [PSignal.Pitch]
    -> PSignal.Pitch -> Derive.Deriver (Maybe Attrs.Attributes)
grace_attributes pos supported [grace] base = do
    diff <- Call.nn_difference pos base grace
    return $ Map.lookup (round diff) supported
grace_attributes _ _ _ _ = return Nothing

-- * util

legato_grace :: Derive.NoteArgs -> Signal.Y -> [PSignal.Pitch]
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.NoteDeriver
legato_grace args dyn_scale pitches grace_dur place = do
    dyn <- (*dyn_scale) <$> (Call.dynamic =<< Args.real_start args)
    events <- basic_grace_transform args pitches (Call.with_dynamic dyn)
        grace_dur place
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.with_val "legato-dyn" (1 :: Double) $
        Sub.reapply_call (Args.context args) "(" [] [events]

basic_grace_dyn :: Signal.Y -> Derive.PassedArgs a -> [PSignal.Pitch]
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.NoteDeriver
basic_grace_dyn dyn_scale args pitches grace_dur place = do
    dyn <- (*dyn_scale) <$> (Call.dynamic =<< Args.real_start args)
    Sub.derive =<< basic_grace_transform args pitches (Call.with_dynamic dyn)
        grace_dur place

basic_grace_transform :: Derive.PassedArgs a -> [PSignal.Pitch]
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> BaseTypes.Duration -> BaseTypes.ControlRef -> Derive.Deriver [Sub.Event]
basic_grace_transform args pitches transform = basic_grace args notes
    where notes = map (transform . Call.pitched_note) pitches ++ [Call.note]

basic_grace :: Derive.PassedArgs a -> [note]
    -> BaseTypes.Duration -> BaseTypes.ControlRef
    -> Derive.Deriver [Sub.GenericEvent note]
basic_grace args pitches =
    make_grace_notes (Args.prev_start args) (Args.range_or_next args) pitches

-- | Determine grace note starting times and durations if they are to fit in
-- the given time range, shortening them if they don't fit.
fit_grace_durs :: (Fractional a, Ord a) => a -> Maybe a -> a -> a -> Int -> a
    -> [(a, a)]
fit_grace_durs place prev start end notes dur =
    map add_dur $ Seq.zip_next $ fit_grace place prev start end notes dur
    where
    add_dur (x, Nothing) = (x, end - x)
    add_dur (x, Just next) = (x, next - x)

fit_grace :: (Fractional a, Ord a) => a -- ^ placement, see 'grace_place_doc'
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

resolve_pitches :: PSignal.Pitch
    -> [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
    -> Derive.Deriver [PSignal.Pitch]
resolve_pitches base = either Derive.throw return . check_pitches base

check_pitches :: PSignal.Pitch -> [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
    -> Either Text [PSignal.Pitch]
check_pitches base pitches = do
    make <- case map ScoreT.type_of $ Either.rights pitches of
        t_ : ts_
            | all (==t) ts -> case t of
                ScoreT.Diatonic -> Right Pitch.Diatonic
                ScoreT.Chromatic -> Right Pitch.Chromatic
                ScoreT.Nn -> Right Pitch.Nn
                _ -> Left $ "expected transpose type, but got " <> pretty t
            | otherwise ->
                Left $ "arguments should all have the same type, got "
                    <> pretty (t:ts)
            where
            t = deflt ScoreT.Diatonic t_
            ts = map (deflt t) ts_
        [] -> Right Pitch.Diatonic
    return $ map (either id (resolve make . ScoreT.typed_val)) pitches
    where
    resolve make n = Pitches.transpose (make n) base
    deflt typ ScoreT.Untyped = typ
    deflt _ typ = typ
