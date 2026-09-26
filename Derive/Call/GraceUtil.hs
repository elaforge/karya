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
import qualified Util.Lists as Lists

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


-- * standard args

graceEnvs :: Sig.Parser (DeriveT.Duration, Typecheck.Normalized)
graceEnvs = (,) <$> graceDurEnv <*> gracePlaceEnv

graceDurEnv :: Sig.Parser DeriveT.Duration
graceDurEnv = Typecheck._real <$>
    Sig.environ "dur" Sig.Both defaultGraceDur "Duration of grace notes."

graceDynEnv :: Sig.Parser Double
graceDynEnv =
    Typecheck.nonNegative <$> Sig.environ "grace-dyn" Sig.Unprefixed
        (0.5 :: Double) "Scale the dyn of the grace notes."

gracePlaceEnv :: Sig.Parser Typecheck.Normalized
gracePlaceEnv = Sig.environ "place" Sig.Both (0 :: Double) gracePlaceDoc

gracePlaceDoc :: Doc.Doc
gracePlaceDoc =
    "At 0, grace notes fall before their base note.  At 1, grace notes fall on\
    \ the base note, and the base note is delayed."

-- | It's pretty much arbitrary, but this seems ok.
defaultGraceDur :: Typecheck.DefaultReal
defaultGraceDur = Typecheck.real (1/12)

gracePitchesArg :: Sig.Parser [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
gracePitchesArg = Sig.many "pitch" gracePitchesDoc

gracePitchesDoc :: Doc.Doc
gracePitchesDoc = "Grace note pitches. If they are numbers,\
    \ they are taken as transpositions and must all be the same type,\
    \ defaulting to diatonic."

-- * calls

-- | Make a grace call with the standard arguments.
makeGrace :: Module.Module -> Doc.Doc
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> (Derive.PassedArgs Score.Event -> [SubT.Event] -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
makeGrace module_ doc transform derive =
    Derive.generator module_ "grace" (Tags.ornament <> Tags.ly) doc
    $ Sig.call ((,,)
    <$> gracePitchesArg <*> graceDynEnv <*> graceEnvs
    ) $ \(pitches, dyn, (graceDur, place)) -> Sub.inverting $ \args -> do
        start <- Args.realStart args
        base <- Call.getPitch start
        pitches <- resolvePitches base pitches
        Ly.whenLilypond (lilyGrace args start pitches) $ do
            withDyn <- (*dyn) <$> (Call.dynamic =<< Args.realStart args)
            derive args =<< basicGraceTransform args pitches
                (transform . Call.withDynamic withDyn) graceDur place

-- | This is like 'make_grace', but gives you pitches instead of realized
-- events, in case you want to merge them or something.
makeGracePitch :: Module.Module -> Doc.Doc
    -> (Derive.PassedArgs Score.Event -> [SubT.EventT PSignal.Pitch]
        -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
makeGracePitch module_ doc derive =
    Derive.generator module_ "grace" (Tags.ornament <> Tags.ly) doc
    $ Sig.call ((,)
    <$> gracePitchesArg <*> graceEnvs
    ) $ \(pitches, (graceDur, place)) -> Sub.inverting $ \args -> do
        start <- Args.realStart args
        base <- Call.getPitch start
        pitches <- resolvePitches base pitches
        Ly.whenLilypond (lilyGrace args start pitches) $ do
            here <- Call.getPitchHere args
            notes <- basicGrace args (pitches ++ [here]) graceDur place
            derive args notes

repeatNotes :: Derive.NoteDeriver -> Int -> DeriveT.Duration
    -> Typecheck.Normalized -- ^ placement, 'grace_place_doc'
    -> Derive.PassedArgs a -> Derive.Deriver [SubT.Event]
repeatNotes note times time place args =
    makeGraceNotes (Args.prevStart args)
        (Args.rangeOrNext args) (replicate times note) time place

makeGraceNotes :: Maybe ScoreTime -> (ScoreTime, ScoreTime) -- ^ (start, end)
    -> [note] -- ^ the last note is the destination
    -> DeriveT.Duration
    -> Typecheck.Normalized -- ^ placement, see 'grace_place_doc'
    -> Derive.Deriver [SubT.EventT note]
makeGraceNotes prev (start, end) notes graceDur place = do
    case graceDur of
        DeriveT.ScoreDuration graceDur -> do
            let extents = fitGraceDurs place prev start end (length notes)
                    graceDur
            pure [SubT.EventT s d n | ((s, d), n) <- zip extents notes]
        DeriveT.RealDuration graceDur -> do
            realStart <- Derive.real start
            realEnd <- Derive.real end
            realPrev <- maybe (pure Nothing) ((Just <$>) . Derive.real) prev
            let extents = fitGraceDurs place realPrev realStart realEnd
                    (length notes) graceDur
            zipWithM noteReal extents notes
    where
    noteReal (start, dur) note = do
        scoreStart <- Derive.score start
        scoreEnd <- Derive.score (start + dur)
        pure $ SubT.EventT scoreStart (scoreEnd - scoreStart) note

lilyGrace :: Derive.PassedArgs d -> RealTime -> [PSignal.Pitch]
    -> Derive.NoteDeriver
lilyGrace args start pitches = do
    env <- Derive.getEnviron
    pitches <- mapM (Ly.pitchToLily env)
        =<< mapM (Derive.resolvePitch start) pitches
    let lyNotes = map (<> Lilypond.toLily Lilypond.D8) pitches
        beamed = Lists.firstLast (<>"[") (<>"]") lyNotes
        -- I use \acciaccatura instead of \grace because it adds a slur
        -- automatically.
        code = "\\acciaccatura { " <> Text.unwords beamed <> " } "
    -- Prepending to the note instead of emitting a separate code event ensures
    -- it stays with the note's voice.
    Ly.addFirst (Ly.prepend, code) $ Call.place args Call.note

-- * attr grace

cAttrGrace :: Map Int Attrs.Attributes
    -- ^ Map intervals in semitones (positive or negative) to attrs.
    -> Derive.Generator Derive.Note
cAttrGrace supported =
    Derive.generator Module.instrument "grace" (Tags.ornament <> Tags.ly)
    ("Emit grace notes as attrs, given a set of possible interval attrs.\
    \ If the grace note can't be expressed by the supported attrs, then emit\
    \ notes like the normal grace call.\nSupported: "
    <> Doc.commas (map ShowVal.doc (Map.elems supported))
    ) $ Sig.call ((,,,)
    <$> gracePitchesArg <*> graceDynEnv <*> graceEnvs
    <*> Sig.environ "attr" Sig.Prefixed (Nothing :: Maybe Sig.Dummy)
        "If given, put this attr on the grace notes. Otherwise, pick a grace\
        \ note from the support list."
    ) $ \(pitches, dyn, (graceDur, place), attr) -> Sub.inverting $ \args -> do
        start <- Args.realStart args
        base <- Call.getPitch start
        pitches <- resolvePitches base pitches
        Ly.whenLilypond (lilyGrace args start pitches) $ case attr of
            Just attr -> Call.addAttributes attr $
                basicGraceDyn dyn args pitches graceDur place
            Nothing -> do
                maybeAttrs <- graceAttributes start supported pitches base
                case maybeAttrs of
                    Just attrs -> attrGrace start args graceDur
                        (length pitches) attrs
                    -- Fall back on normal grace.
                    Nothing -> legatoGrace args dyn pitches graceDur place
    where
    attrGrace realStart args graceDur notes attrs = do
        let (start, dur) = Args.extent args
        graceDur <- Call.scoreDuration start graceDur
        dyn <- Call.dynamic realStart
        let before = fromIntegral notes * graceDur
        pitch <- Call.getPitch realStart
        Derive.place (start - before) (dur + before) $
            Call.addAttributes attrs $ Call.withDynamic dyn $
            Call.pitchedNote pitch

graceAttributes :: RealTime -> Map Int Attrs.Attributes -> [PSignal.Pitch]
    -> PSignal.Pitch -> Derive.Deriver (Maybe Attrs.Attributes)
graceAttributes pos supported [grace] base = do
    diff <- Call.nnDifference pos base grace
    pure $ Map.lookup (round diff) supported
graceAttributes _ _ _ _ = pure Nothing

-- * util

legatoGrace :: Derive.NoteArgs -> Signal.Y -> [PSignal.Pitch]
    -> DeriveT.Duration -> Typecheck.Normalized -> Derive.NoteDeriver
legatoGrace args dynScale pitches graceDur place = do
    dyn <- (*dynScale) <$> (Call.dynamic =<< Args.realStart args)
    events <- basicGraceTransform args pitches (Call.withDynamic dyn)
        graceDur place
    -- Normally legato notes emphasize the first note, but that's not
    -- appropriate for grace notes.
    Derive.withVal "legato-dyn" (1 :: Double) $
        Sub.reapplyCall (Args.context args) "(" [] [events]

basicGraceDyn :: Signal.Y -> Derive.PassedArgs a -> [PSignal.Pitch]
    -> DeriveT.Duration -> Typecheck.Normalized -> Derive.NoteDeriver
basicGraceDyn dynScale args pitches graceDur place = do
    dyn <- (*dynScale) <$> (Call.dynamic =<< Args.realStart args)
    Sub.derive =<< basicGraceTransform args pitches (Call.withDynamic dyn)
        graceDur place

basicGraceTransform :: Derive.PassedArgs a -> [PSignal.Pitch]
    -> (Derive.NoteDeriver -> Derive.NoteDeriver)
    -> DeriveT.Duration -> Typecheck.Normalized -> Derive.Deriver [SubT.Event]
basicGraceTransform args pitches transform = basicGrace args notes
    where notes = map (transform . Call.pitchedNote) pitches ++ [Call.note]

basicGrace :: Derive.PassedArgs a -> [note]
    -> DeriveT.Duration -> Typecheck.Normalized
    -> Derive.Deriver [SubT.EventT note]
basicGrace args pitches =
    makeGraceNotes (Args.prevStart args) (Args.rangeOrNext args) pitches

-- | Determine grace note starting times and durations if they are to fit in
-- the given time range, shortening them if they don't fit.
fitGraceDurs :: (Fractional a, Ord a) => Typecheck.Normalized -> Maybe a
    -> a -> a -> Int -> a -> [(a, a)]
fitGraceDurs place prev start end notes dur =
    map addDur $ Lists.zipNext $ fitGrace place prev start end notes dur
    where
    addDur (x, Nothing) = (x, end - x)
    addDur (x, Just next) = (x, next - x)

fitGrace :: (Fractional a, Ord a)
    => Typecheck.Normalized -- ^ placement, see 'grace_place_doc'
    -> Maybe a -> a -> a -> Int -> a -> [a]
fitGrace (Typecheck.Normalized place) maybePrev start end notes dur
    | place <= 0 = before
    | place >= 1 = after
    | otherwise =
        zipWith (\x y -> Num.scale x y (realToFrac place)) before after
    where
    after = fitAfter start end notes dur
    before = fitBefore maybePrev start notes dur

fitBefore :: (Fractional a, Ord a) => Maybe a -> a -> Int -> a -> [a]
fitBefore maybePrev start notes dur =
    take notes $ drop 1 $ Lists.range_ (start - notesT * step) step
    where
    notesT = fromIntegral notes
    step
        | Just prev <- maybePrev, start - dur * notesT < prev =
            (start - prev) / notesT
        | otherwise = dur

fitAfter :: (Fractional a, Ord a) => a -> a -> Int -> a -> [a]
fitAfter start end notes dur = take notes $ Lists.range_ start step
    where
    notesT = fromIntegral notes
    step
        | dur * notesT >= end - start = (end - start) / notesT
        | otherwise = dur

resolvePitches :: PSignal.Pitch
    -> [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
    -> Derive.Deriver [PSignal.Pitch]
resolvePitches base = either Derive.throw pure . checkPitches base

checkPitches :: PSignal.Pitch -> [Either PSignal.Pitch (ScoreT.Typed Signal.Y)]
    -> Either Text [PSignal.Pitch]
checkPitches base pitches = do
    make <- case map ScoreT.typeOf $ Either.rights pitches of
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
    pure $ map (either id (resolve make . ScoreT.valOf)) pitches
    where
    resolve make n = Pitches.transpose (make n) base
    deflt typ ScoreT.Untyped = typ
    deflt _ typ = typ
