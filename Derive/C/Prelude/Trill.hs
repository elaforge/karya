-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Various kinds of trills.

    Trills want to generate an integral number of cycles.  For the purpose of
    counting integral cycles, trills count the end (either the end of the
    event, or the start of the next event).  This is different than other
    control calls, which tend to omit the end point, expecting that the next
    call will place a sample there.  This is so that a trill can end on an off
    note if it exactly fits into its allotted space, otherwise a 16th note
    trill in a quarter note would degenerate into a mordent.

    Various flavors of trills:

    - Trill cycles depend on real duration of note.  Cycle durations are given
    in real time.

    - As above, but durations are given in score time.

    - Number of trill cycles given as argument, and note stretches normally.

    - Sung style vibrato in a sine wave rather than a square wave.

    - Trill that simply adds an attribute, instrument will handle it.

    The generic 'tr' symbol can be bound to whichever variant is locally
    appropriate.

    It's easy to think of more variants of trills: hold the starting note
    briefly, hold the final note briefly, inject a little randomness, smooth
    the pitch curve by a variable amount, or variants that cover the range
    between trill and vibrato, etc.  One can also imagine dynamic effects.

    Instead of trying to provide a million functions here or a few
    with a million parameters, it should be relatively easy to reuse the
    functions in here to write a specific kind of trill for the particular
    piece.
-}
module Derive.C.Prelude.Trill (
    library
    , cNoteTrill, cTremoloGenerator
    , holdEnv, tremoloStartsCurve
    -- * transitions
    , trillTransitions, adjustedTransitions
    -- * types
    , Config(..)
    , Direction(..), directionAffix
    , AbsoluteMode(..)
    , Adjust(..), adjustEnv
    , getTrillControlSmooth

    -- testing
    , fullNotes, chordTremolo, getTrillControl, xcutControl
) where
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Convert as Lilypond.Convert
import qualified Perform.Lilypond.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


library :: Library.Library
library = mconcat
    -- Note
    [ makeTrills "tr" (cNoteTrill False)
    , Library.generators [("trem", cTremoloGenerator Nothing)]
    , Library.transformers [("trem", cTremoloTransformer)]
    -- Pitch
    , makeTrills "tr" cPitchTrill
    , makeTrills "trs" cPitchTrillSmooth
    , Library.generators
        [ ("xcut", cXcutPitch False)
        , ("xcut-h", cXcutPitch True)
        ]
    -- Control
    , makeTrills "tr" cControlTrill
    , makeTrills "trs" cControlTrillSmooth
    , Library.generators $
        [ ("saw", cSaw)
        , ("sine", cSine Bipolar)
        , ("sine+", cSine Positive)
        , ("sine-", cSine Negative)
        , ("xcut", cXcutControl False)
        , ("xcut-h", cXcutControl True)
        ]
    ]
    where
    makeTrills prefix make = Library.generators
        [(name, make start end) | (name, start, end) <- trillVariations prefix]

-- * note calls

cNoteTrill :: Bool -> Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Note
cNoteTrill useAttributes hardcodedStart hardcodedEnd =
    Derive.generator Module.prelude "tr" Tags.ly
    ("Generate a note with a trill.\
    \\nUnlike a trill on a pitch track, this generates events for each\
    \ note of the trill. This is more appropriate for fingered trills,\
    \ or monophonic instruments that use legato to play slurred notes.\
    \\nInstruments that support +trill attributes should enable the attributes\
    \ version, which emits a single note with `+trill+half`, `+trill+whole`, or\
    \ all the notes with `+trill`, depending on the interval."
    <> directionDoc hardcodedStart hardcodedEnd
    ) $ Sig.call ((,,)
    <$> neighborArg
    <*> configArg hardcodedStart hardcodedEnd
    <*> Sig.environKey "tr-style" Tr
        "Notation variant: tr symbol, tr~, or tremolo."
    ) $ \(neighbor, config, style) -> Sub.inverting $ \args ->
    Ly.whenLilypond (noteTrillLy style args neighbor)
        (noteTrill useAttributes neighbor config args)

type Neighbor = Either Typecheck.DiatonicTransposeFunctionT PSignal.Pitch

neighborArg :: Sig.Parser Neighbor
neighborArg = Sig.defaulted "neighbor" (Left 1 :: Either Int Sig.Dummy)
    "Alternate with an interval or pitch."

noteTrill :: Bool -> Neighbor -> Config
    -> Derive.PassedArgs a -> Derive.NoteDeriver
noteTrill useAttributes neighbor config args
    | useAttributes = trillAttributes neighbor (Args.start args) >>= \case
        Just attr ->
            Call.addAttributes (Attrs.trill <> attr) (Call.placedNote args)
        Nothing -> Call.addAttributes Attrs.trill trillNotes
    | otherwise = trillNotes
    where
    trillNotes = do
        (neighbor, control) <- neighborToFunction (Args.start args) neighbor
        transpose <- getTrillControl config (Args.rangeOrNext args) neighbor
        Sub.derive =<< mapM (note control) (Lists.zipNext transpose)
    note control ((x, transpose), next) = do
        start <- Derive.score x
        let end = snd $ Args.range args
        next <- maybe (pure end) (Derive.score . fst) next
        pure $ SubT.EventT start (next-start) $
            Call.addConstant control transpose Call.note
    -- trill_notes = do
    --     neighbor <- neighbor_to_signal2 (Args.start args) neighbor
    --     (transpose, control) <- get_trill_control2
    --         (Args.range_or_next args) start_dir end_dir adjust hold
    --         neighbor speed
    --     xs <- mapM (Derive.score . fst) transpose
    --     let end = snd $ Args.range args
    --     let notes = do
    --             (x, maybe_next) <- Lists.zipNext xs
    --             let next = fromMaybe end maybe_next
    --             return $ SubT.EventT x (next-x) Call.note
    --     Call.add_control control (ScoreT.untyped transpose)
    --         (Sub.derive notes)

        -- TODO this is an implementation that directly uses the neighbor pitch
        -- instead of the roundabout signal thing.  But I still need the signal
        -- if it changes.  Implement when I'm not in such a hurry.

        -- neighbor <- case neighbor of
        --     Right p -> return p
        --     Left control -> undefined
        -- let neighbor_low = False -- TODO
        -- (who_first, transitions) <- get_trill_transitions
        --     (Args.range_or_next args) start_dir end_dir
        --     adjust hold speed neighbor_low
        -- base <- Call.get_pitch_here args
        -- let pitches = cycle $ case who_first of
        --         Unison -> [base, neighbor]
        --         Neighbor -> [neighbor, base]
        -- transitions <- mapM Derive.score transitions
        -- Sub.derive $ do
        --     (pitch, (x, maybe_next)) <-
        --         zip pitches (Lists.zipNext transitions)
        --     let next = fromMaybe (snd (Args.range args)) maybe_next
        --     return $ SubT.EventT x (next-x) (Call.pitched_note pitch)

neighborToFunction :: ScoreTime -> Neighbor
    -> Derive.Deriver (ScoreT.Function, ScoreT.Control)
neighborToFunction _ (Left (Typecheck.DiatonicTransposeFunctionT typ f)) =
    pure (f, Typecheck.transposeControl typ)
neighborToFunction start (Right neighbor) = do
    start <- Derive.real start
    base <- Call.getPitch start
    diff <- Call.nnDifference start neighbor base
    pure (const (realToFrac diff), Typecheck.transposeControl ScoreT.TNn)

trillAttributes :: Neighbor -> ScoreTime
    -> Derive.Deriver (Maybe Attrs.Attributes)
trillAttributes neighbor start = do
    start <- Derive.real start
    (pitch, neighbor) <- pitchAndNeighbor neighbor start
    diff <- Call.nnDifference start neighbor pitch
    pure $ if
        | Pitch.nnsEqual diff 1 -> Just Attrs.half
        | Pitch.nnsEqual diff 2 -> Just Attrs.whole
        | otherwise -> Nothing

data TrillStyle = Tr | Span | Tremolo
    deriving (Show, Eq, Enum, Bounded)

instance Typecheck.Typecheck TrillStyle
instance Typecheck.ToVal TrillStyle
instance ShowVal.ShowVal TrillStyle

noteTrillLy :: TrillStyle -> Derive.PassedArgs a -> Neighbor
    -> Derive.NoteDeriver
noteTrillLy style args neighbor = do
    start <- Args.realStart args
    (pitch, neighbor) <- pitchAndNeighbor neighbor start
    diff <- Call.nnDifference start neighbor pitch
    let tremolo = tremoloTrillLy pitch neighbor (Args.start args)
            (Args.duration args)
    env <- Derive.getEnviron
    let lyPitch = Derive.requireRight id . Lilypond.Convert.pitchToLily env
            =<< Derive.resolvePitch start neighbor
    case style of
        _ | not (Pitch.nnsEqual diff 1) && not (Pitch.nnsEqual diff 2) ->
            tremolo
        Tremolo -> tremolo
        Tr -> do
            npitch@(Types.Pitch _ _ acc) <- lyPitch
            inKey <- pitchInKey npitch
            let code = case acc of
                    _ | inKey -> "\\trill"
                    Types.FlatFlat -> "^\\trFlatFlat"
                    Types.Flat -> "^\\trFlat"
                    Types.Natural -> "^\\trNatural"
                    Types.Sharp -> "^\\trSharp"
                    Types.SharpSharp -> "^\\trSharpSharp"
            -- TODO this should by Ly.note_append, but I can't put the \trFlat
            -- macros on a single pitch.
            Ly.addFirst (Ly.append Constants.All, code)
                (Call.placedNote args)
        Span -> do
            npitch <- lyPitch
            Ly.addFirst (Ly.prepend, "\\pitchedTrill") $
                Ly.addFirst (Ly.append Constants.First, "\\startTrillSpan "
                    <> Types.toLily npitch) $
                Ly.addFirst (Ly.append Constants.Last, "\\stopTrillSpan") $
                Call.placedNote args

pitchInKey :: Types.Pitch -> Derive.Deriver Bool
pitchInKey lyPitch = do
    key <- getKey
    pure $ inKey key (Pitch.pitchDegree (Types.toPitch lyPitch))

getKey :: Derive.Deriver Theory.Key
getKey = do
    maybeKey <- Call.lookupKey
    Derive.require ("unrecognized key: " <> pretty maybeKey) $
        Twelve.lookupKey maybeKey

inKey :: Theory.Key -> Pitch.Degree -> Bool
inKey key (Pitch.Degree pc acc) = Theory.accidentalsAtPc key pc == acc

pitchAndNeighbor :: Neighbor -> RealTime
    -> Derive.Deriver (PSignal.Pitch, PSignal.Pitch)
pitchAndNeighbor (Right neighbor) start =
    (, neighbor) <$> Call.getPitch start
pitchAndNeighbor (Left (Typecheck.DiatonicTransposeFunctionT typ f))
        start = do
    base <- Call.getPitch start
    let width = f start
    case (typ, width) of
        (ScoreT.TChromatic, 1) ->
            (base,) <$> (chromaticNeighbor =<< Derive.resolvePitch start base)
        _ -> pure
            (base, Pitches.transpose (Typecheck.toTranspose typ width) base)

-- | Given a pitch, find the enharmonic one chromatic step above it which is
-- at pitch class + 1.  This is because trills should alternate with the next
-- pitch class, so c to d flat, not c to c#.
chromaticNeighbor :: PSignal.Transposed -> Derive.Deriver PSignal.Pitch
chromaticNeighbor pitch = do
    -- TODO this is way too complicated
    (parse, unparse, transpose) <- Call.getPitchFunctions
    pitch <- Call.parsePitch parse pitch
    key <- getKey
    neighbor <- Derive.require "transpose" $ transpose Scale.Chromatic 1 pitch
    neighbor <- Derive.require "enharmonic" $
        List.find ((== Pitch.pitchPc pitch + 1) . Pitch.pitchPc) $
            Theory.enharmonicsOf (Theory.keyLayout key) neighbor
    note <- Derive.require "unparse" $ unparse neighbor
    scale <- Call.getScale
    Eval.evalNote scale note

-- | Emit the magic events to trigger lilypond's tremolo processing.
tremoloTrillLy :: PSignal.Pitch -> PSignal.Pitch -> ScoreTime -> ScoreTime
    -> Derive.NoteDeriver
tremoloTrillLy pitch1 pitch2 start dur =
    Derive.place start dur $ mconcat
        [ Ly.addFirst (Ly.SetEnviron Constants.vTremolo, "") Call.note
        , Call.pitchedNote pitch1
        , Call.pitchedNote pitch2
        ]

cTremoloGenerator :: Maybe ([Attrs.Attributes], Attrs.Attributes)
    -> Derive.Generator Derive.Note
cTremoloGenerator attrsUnless =
    Derive.generator Module.prelude "trem" Tags.ly
    ("Repeat a single note. Or, if there are sub-notes, alternate with each of\
    \ the sub-notes in turn." <> case attrsUnless of
        Nothing -> ""
        Just (unless, _) -> if attrsUnless == Nothing then "" else
            "\nThis version just derives plain notes with the "
            <> ShowVal.doc Attrs.trem <> " attribute, unless any of these\
            \ attributes are present: " <> ShowVal.doc unless <> ".")
    $ Sig.call ((,) <$> Speed.arg <*> holdEnv) $ \(speed, hold) args -> do
        starts <- tremoloStarts hold speed (Args.rangeOrNext args)
        notes <- Sub.subEvents args
        attrs <- Call.getAttributes
        let useAttrs = maybe False (not . any (Attrs.contain attrs). fst)
                attrsUnless
        let tremAttrs = maybe mempty snd attrsUnless
        case filter (not . null) notes of
            [] -> Sub.invertingArgs args $ \args -> Ly.noteCode code args $
                if useAttrs
                    then Call.addAttributes Attrs.trem Call.note
                    else Call.addAttributes tremAttrs $
                        simpleTremolo starts [Call.note]
            notes -> Ly.notesCode code args $ if useAttrs
                then Call.addAttributes Attrs.trem $ Sub.deriveTracks notes
                else Call.addAttributes tremAttrs $
                    Sub.derive $ chordTremolo starts notes
    where
    code = (Ly.append Constants.All, ":32")

cTremoloTransformer :: Derive.Transformer Derive.Note
cTremoloTransformer = Derive.transformer Module.prelude "trem" Tags.subs
    "Repeat the transformed note. The generator is creating the notes so it\
    \ can set them to the appropriate duration, but this one has to stretch\
    \ them to fit." $ Sig.callt ((,) <$> Speed.arg <*> holdEnv) $
    \(speed, hold) args deriver -> do
        starts <- tremoloStarts hold speed (Args.rangeOrNext args)
        simpleTremolo starts [Args.normalized args deriver]

tremoloStarts :: DeriveT.Duration -> Typecheck.RealTimeFunctionT
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremoloStarts hold (Typecheck.RealTimeFunctionT ttype speed) (start, end) = do
    hold <- Call.scoreDuration start hold
    addHold (start, end) hold <$> case ttype of
        ScoreT.TReal -> do
            start <- Derive.real (start + hold)
            end <- Derive.real end
            mapM Derive.score . fullNotes end
                =<< Speed.realStarts speed start end
        ScoreT.TScore -> do
            starts <- Speed.scoreStarts speed (start + hold) end
            pure $ fullNotes end starts

-- | This is like 'tremolo_starts', but takes a start and end speed instead
-- of a speed signal.  In exchange, it can have start and end be different
-- time types, which a signal can't express.  Of course I could make the
-- signal into duration and then do the reciprocal in the score as a val call,
-- but that seems too complicated for tracklang.
tremoloStartsCurve :: ControlUtil.CurveF -> DeriveT.Duration
    -> Speed.Speed -> Speed.Speed -> (ScoreTime, ScoreTime)
    -> Derive.Deriver [ScoreTime]
    -- ^ start time for each note, and one for the end of the last one
tremoloStartsCurve curvef hold startSpeed endSpeed (start, end) = do
    hold <- Call.scoreDuration start hold
    realRange <- (,) <$> Derive.real start <*> Derive.real end
    (addHold (start, end) hold . fullNotes end <$>) $
        mapM Derive.score =<< Speed.startsCurve curvef startSpeed endSpeed
            realRange includeEnd
    where includeEnd = True -- because the end time is also included.

-- | Add the hold time to the first tremolo note.
addHold :: (ScoreTime, ScoreTime) -> ScoreTime -> [ScoreTime] -> [ScoreTime]
addHold (start, end) hold starts
    | hold >= end - start = [start, end]
    | hold > 0 = start : starts
    | otherwise = starts

-- | Alternate each note with the other notes within its range, in order from
-- the lowest track to the highest.
--
-- This doesn't restart the tremolo when a new note enters, if you want that
-- you can have multiple tremolo events.
chordTremolo :: forall a. [ScoreTime] -> [[SubT.EventT a]] -> [SubT.EventT a]
chordTremolo starts noteTracks =
    Maybe.catMaybes $ snd $
        List.mapAccumL emit (-1, byTrack) $ zip starts (drop 1 starts)
    where
    emit (lastTracknum, notes_) (pos, nextPos) = case chosen of
        Nothing -> ((lastTracknum, notes), Nothing)
        Just (tracknum, note) -> ((tracknum, notes),
            Just $ SubT.EventT pos (nextPos-pos) (SubT._note note))
        where
        chosen =
            Lists.minimumOn fst (filter ((>lastTracknum) . fst) overlapping)
                <|> Lists.minimumOn fst overlapping
        overlapping = filter (SubT.overlaps pos . snd) notes
        notes = dropWhile ((<=pos) . SubT.end . snd) notes_
    byTrack :: [(TrackNum, SubT.EventT a)]
    byTrack = Lists.sortOn (SubT.end . snd)
        [ (tracknum, event)
        | (tracknum, track) <- zip [0..] noteTracks, event <- track
        ]

-- | Just cycle the given notes.
simpleTremolo :: [ScoreTime] -> [Derive.NoteDeriver] -> Derive.NoteDeriver
simpleTremolo starts notes = Sub.derive
    [ SubT.EventT start (end - start) note
    | (start, end, note) <- zip3 starts (drop 1 starts) $
        if null notes then [] else cycle notes
    ]

-- | Given start times, return only ones whose full duration fits before the
-- end time.  This is the tremolo analog to 'full_cycles'.  Unlike a trill, it
-- emits both the starts and ends, and therefore the last sample will be at the
-- end time, rather than before it.  It should always emit an even number of
-- elements.
fullNotes :: Ord a => a -> [a] -> [a]
fullNotes end [t]
    | t < end = [t, end]
    | otherwise = []
fullNotes end ts = go ts
    where
    go [] = []
    go (t1:ts) = case ts of
        t2 : _
            | t2 > end -> [end]
            | otherwise -> t1 : go ts
        [] -> [end]
    -- This is surprisingly tricky.


-- * pitch calls

cPitchTrill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Pitch
cPitchTrill hardcodedStart hardcodedEnd =
    Derive.generator1 Module.prelude "tr" mempty
    ("Generate a pitch signal of alternating pitches."
    <> directionDoc hardcodedStart hardcodedEnd
    ) $ Sig.call ((,,,,)
    <$> Sig.required "note" "Base pitch."
    <*> neighborArg
    <*> configArg hardcodedStart hardcodedEnd
    <*> ControlUtil.curveEnv
    <*> transitionEnv
    ) $ \(note, neighbor, config, curve, transition) args -> do
        (neighbor, control) <- neighborToFunction (Args.start args) neighbor
        transpose <- getTrillControl config (Args.rangeOrNext args) neighbor
        transpose <- smoothTrill transition curve transpose
        start <- Args.realStart args
        pure $ PSignal.applyControl control (ScoreT.untyped transpose) $
            PSignal.fromSample start note

cPitchTrillSmooth :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Pitch
cPitchTrillSmooth hardcodedStart hardcodedEnd =
    Derive.generator1 Module.prelude "trs" mempty
    ("Generate a pitch signal of alternating pitches. Like `tr`, but with\
    \ defaults for smooth transitions."
    <> directionDoc hardcodedStart hardcodedEnd
    ) $ Sig.call ((,,)
    <$> Sig.required "note" "Base pitch."
    <*> neighborArg
    <*> configArg hardcodedStart hardcodedEnd
    ) $ \(note, neighbor, config) args -> do
        (neighbor, control) <- neighborToFunction (Args.start args) neighbor
        transpose <- getTrillControlSmooth config curve
            (Args.rangeOrNext args) neighbor
        start <- Args.realStart args
        pure $ PSignal.applyControl control (ScoreT.untyped transpose) $
            PSignal.fromSample start note
    where
    curve = ControlUtil.Function $ ControlUtil.sigmoid 0.5 0.5

cXcutPitch :: Bool -> Derive.Generator Derive.Pitch
cXcutPitch hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two pitches.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> Sig.required "fst" "First pitch."
    <*> Sig.required "snd" "Second pitch."
    <*> Sig.defaulted "speed" (14 :: Int) "Speed."
    ) $ \(xcut1, xcut2, speed) args -> do
        transitions <- Speed.starts speed (Args.rangeOrNext args) False
        pure $ xcutPitch hold xcut1 xcut2 transitions

xcutPitch :: Bool -> PSignal.PSignal -> PSignal.PSignal -> [RealTime]
    -> PSignal.PSignal
xcutPitch hold val1 val2 = mconcat . map slice . zip (cycle [val1, val2])
    where
    slice (val, t)
        | hold = maybe mempty (PSignal.fromSample t) (PSignal.at val t)
        | otherwise = PSignal.clipBefore t val


-- * control calls

cControlTrill :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Control
cControlTrill hardcodedStart hardcodedEnd =
    Derive.generator1 Module.prelude "tr" mempty
    ("The control version of the pitch trill. It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    <> directionDoc hardcodedStart hardcodedEnd
    ) $ Sig.call ((,,)
    <$> Sig.defaulted "neighbor" (1 :: Int) "Alternate with this value."
    <*> configArg hardcodedStart hardcodedEnd
    <*> transitionEnv
    ) $ \(neighbor, config, transition) args -> do
        transpose <- getTrillControl config (Args.rangeOrNext args) neighbor
        smoothTrill transition ControlUtil.Linear transpose

cControlTrillSmooth :: Maybe Direction -> Maybe Direction
    -> Derive.Generator Derive.Control
cControlTrillSmooth hardcodedStart hardcodedEnd =
    Derive.generator1 Module.prelude "tr" mempty
    ("The control version of the pitch trill. It generates a signal of values\
    \ alternating with 0, which can be used as a transposition signal."
    <> directionDoc hardcodedStart hardcodedEnd
    ) $ Sig.call ((,)
    <$> Sig.defaulted "neighbor" (1 :: Int) "Alternate with this value."
    <*> configArg hardcodedStart hardcodedEnd
    ) $ \(neighbor, config) args -> do
        transpose <- getTrillControlSmooth config curve
            (Args.rangeOrNext args) neighbor
        pure transpose
    where
    curve = ControlUtil.Function $ ControlUtil.sigmoid 0.5 0.5

cSaw :: Derive.Generator Derive.Control
cSaw = Derive.generator1 Module.prelude "saw" mempty
    "Emit a sawtooth.  By default it has a downward slope, but you can make\
    \ an upward slope by setting `from` and `to`."
    $ Sig.call ((,,)
    <$> Speed.arg
    <*> Sig.defaulted "from" (1 :: Double) "Start from this value."
    <*> Sig.defaulted "to" (0 :: Double) "End at this value."
    ) $ \(speed, from, to) args -> do
        starts <- Speed.starts speed (Args.rangeOrNext args) True
        srate <- Call.getSrate
        pure $ saw srate starts from to

saw :: RealTime -> [RealTime] -> Double -> Double -> Signal.Control
saw srate starts from to = mconcat $ zipWith saw starts (drop 1 starts)
    where saw t1 t2 = ControlUtil.segment srate ControlUtil.Linear t1 from t2 to

-- ** sine

data SineMode = Bipolar | Negative | Positive deriving (Show)

-- | This is probably not terribly convenient to use on its own, I should
-- have some more specialized calls based on this.
cSine :: SineMode -> Derive.Generator Derive.Control
cSine mode = Derive.generator1 Module.prelude "sine" mempty
    "Emit a sine wave. The default version is centered on the `offset`,\
    \ and the `+` and `-` variants are above and below it, respectively."
    $ Sig.call ((,,)
    <$> Sig.defaulted "speed" (1 :: RealTime) "Frequency."
    <*> Sig.defaulted "amp" (1 :: Double) "Amplitude, measured center to peak."
    <*> Sig.defaulted "offset" (0 :: Double) "Center point."
    ) $ \(Typecheck.RealTimeFunctionT timeType speed, amp, offset) args -> do
        case timeType of
            ScoreT.TScore -> Derive.throw "RealTime signal required"
            _ -> pure ()
        srate <- Call.getSrate
        let sign = case mode of
                Bipolar -> 0
                Negative -> -amp
                Positive -> amp
        (start, end) <- Args.realRangeOrNext args
        pure $ Signal.mapYLinear ((+(offset+sign)) . (*amp)) $
            sine srate start end speed

sine :: RealTime -> RealTime -> RealTime -> ScoreT.Function -> Signal.Control
sine srate start end freqSig = Signal.unfoldr go (start, 0)
    where
    go (pos, phase)
        | pos >= end = Nothing
        | otherwise = Just ((pos, sin phase), (pos + 1/srate, nextPhase))
        where
        nextPhase = phase + 1 / RealTime.toSeconds srate * 2*pi * freqSig pos


-- ** xcut

cXcutControl :: Bool -> Derive.Generator Derive.Control
cXcutControl hold = Derive.generator1 Module.prelude "xcut" mempty
    "Cross-cut between two signals.  The `-h` variant holds the value at the\
    \ beginning of each transition."
    $ Sig.call ((,,)
    <$> Sig.defaulted "fst" (1 :: Int) "First value."
    <*> Sig.defaulted "snd" (0 :: Int) "Second value."
    <*> Sig.defaulted "speed" (14 :: Int) "Speed."
    ) $ \(xcut1, xcut2, speed) args -> do
        transitions <- Speed.starts speed (Args.rangeOrNext args) False
        pure $ xcutControl hold xcut1 xcut2 transitions

-- TODO(polymorphic-signals) This is the same as 'xcut_pitch'
xcutControl :: Bool -> Signal.Control -> Signal.Control -> [RealTime]
    -> Signal.Control
xcutControl hold val1 val2 = mconcat . map slice . zip (cycle [val1, val2])
    where
    slice (val, t)
        | hold = Signal.fromSample t (Signal.at val t)
        | otherwise = Signal.clipBefore t val

-- * util

trillSpeedArg :: Sig.Parser Typecheck.RealTimeFunctionT
trillSpeedArg =
    Sig.defaulted "speed" (14 :: Int)
    "Trill at this speed. If it's a RealTime, the value is the number of\
    \ cycles per second, which will be unaffected by the tempo. If it's\
    \ a ScoreTime, the value is the number of cycles per ScoreTime\
    \ unit, and will stretch along with tempo changes. In either case,\
    \ this will emit only whole notes, i.e. it will end sooner to avoid\
    \ emitting a cut-off note at the end."

-- | Whether the trill starts or ends on the high or low note.  This is another
-- way to express 'AbsoluteMode'.
--
-- I had a lot of debate about whether I should use High and Low, or Unison and
-- Neighbor.  Unison-Neighbor is more convenient for the implementation but
-- High-Low I think is more musically intuitive.
data Direction = High | Low deriving (Bounded, Eq, Enum, Show)
instance ShowVal.ShowVal Direction
instance Typecheck.Typecheck Direction
instance Typecheck.ToVal Direction

-- | This is the like 'Direction', but in terms of the unison and neighbor
-- pitches, instead of high and low.
data AbsoluteMode = Unison | Neighbor deriving (Bounded, Eq, Enum, Show)

transitionEnv :: Sig.Parser ScoreT.Function
transitionEnv =
    Sig.environ "tr-transition" Sig.Unprefixed (0 :: Int)
    "Take this long to reach the neighbor, as a proportion of time available."

-- | A bundle of standard configuration for trills.
configArg :: Maybe Direction -> Maybe Direction -> Sig.Parser Config
configArg startDir endDir =
    Config <$> trillSpeedArg <*> start <*> end <*> holdEnv <*> adjustEnv
        <*> bias <*> pure False
    where
    start = case startDir of
        Nothing -> Sig.environ "tr-start" Sig.Unprefixed
            (Nothing :: Maybe Direction)
            "Which note the trill starts with. If not given, it will start\
            \ the unison note, which means it may move up or down."
        Just dir -> pure $ Just dir
    end = case endDir of
        Nothing -> Sig.environ "tr-end" Sig.Unprefixed
            (Nothing :: Maybe Direction)
            "Which note the trill ends with. If not given, it can end with\
            \ either."
        Just dir -> pure $ Just dir
    bias = Typecheck.normalizedBipolar <$>
        Sig.environ "tr-bias" Sig.Unprefixed (Typecheck.NormalizedBipolar 0)
            "Offset every other transition by this amount."

data Config = Config
    -- | transition speed
    { _speed :: !Typecheck.RealTimeFunctionT
    , _startDir :: !(Maybe Direction)
    , _endDir :: !(Maybe Direction)
    -- | extend the first transition by this amount
    , _hold :: !DeriveT.Duration
    -- | how to fit the transitions into the time range
    , _adjust :: !Adjust
    -- | offset every other transition by this amount, from -1--1
    , _bias :: !Double
    -- | include a transition at the end time
    , _includeEnd :: !Bool
    }

-- Its default is both prefixed and unprefixed so you can put in a tr-hold
-- globally, and so you can have a short @hold=n |@ for a single call.
holdEnv :: Sig.Parser DeriveT.Duration
holdEnv = Typecheck._real <$>
    Sig.environ (Derive.ArgName EnvKey.hold) Sig.Both
        (Typecheck.real 0) "Time to hold the first note."

trillVariations :: Text -> [(Expr.Symbol, Maybe Direction, Maybe Direction)]
trillVariations prefix =
    [ (Expr.Symbol $ prefix
            <> (if start == Nothing && end /= Nothing
                then "-" else directionAffix start)
            <> directionAffix end,
        start, end)
    | start <- dirs, end <- dirs
    ]
    where dirs = [Nothing, Just High, Just Low]

directionAffix :: Maybe Direction -> Text
directionAffix Nothing = ""
directionAffix (Just High) = "^"
directionAffix (Just Low) = "_"

directionDoc :: Maybe a -> Maybe a -> Doc.Doc
directionDoc Nothing Nothing = ""
directionDoc _ _ = "\nA `^` suffix makes the trill starts on the higher value,\
    \ while `_` makes it start on the lower value. A second suffix causes it\
    \ to end on the higher or lower value, e.g. `^_` starts high and ends low.\
    \ `-_` has start unspecified, and ends low.\
    \ No suffix causes it to obey the settings in scope."

-- | How to adjust an ornament to fulfill its 'Direction' restrictions.
data Adjust
    -- | Adjust by shortening the ornament.
    = Shorten
    -- | Adjust by increasing the speed.
    | Stretch
    deriving (Bounded, Eq, Enum, Show)

instance ShowVal.ShowVal Adjust
instance Typecheck.Typecheck Adjust
instance Typecheck.ToVal Adjust

adjustEnv :: Sig.Parser Adjust
adjustEnv = Sig.environ "adjust" Sig.Both Shorten
    "How to adjust a trill to fulfill its start and end pitch restrictions."

-- ** transitions

-- | A signal that alternates between the base and neighbor values.
getTrillControl :: Config -> (ScoreTime, ScoreTime)
    -> ScoreT.Function -> Derive.Deriver [(RealTime, Signal.Y)]
getTrillControl config (start, end) neighbor = do
    realStart <- Derive.real start
    let neighborLow = neighbor realStart < 0
    (whoFirst, transitions) <- getTrillTransitions config (start, end)
        neighborLow
    let (val1, val2) = case whoFirst of
            Unison -> (const 0, neighbor)
            Neighbor -> (neighbor, const 0)
    pure $ trillFromTransitions val1 val2 realStart transitions

-- | Like 'get_trill_control', but for a curved trill.
getTrillControlSmooth :: Config
    -> ControlUtil.Curve -> (ScoreTime, ScoreTime) -> ScoreT.Function
    -> Derive.Deriver Signal.Control
getTrillControlSmooth config curve range neighbor = do
    transpose <- getTrillControl
        -- Trills usually omit the transition that coincides with the end
        -- because that would create a zero duration note.  But these
        -- trills are smoothed and thus will still have a segment leading
        -- to the cut-off transition.
        (config { _includeEnd = True })
        range neighbor
    signal <- smoothTrill (const 1) curve transpose
    pure signal


-- | The points in time where the trill should transition between pitches.
getTrillTransitions :: Config -> (ScoreTime, ScoreTime) -> Bool
    -> Derive.Deriver (AbsoluteMode, [RealTime])
getTrillTransitions config (start, end) neighborLow = do
    let (whoFirst, evenTransitions) =
            convertDirection neighborLow (_startDir config) (_endDir config)
    hold <- Call.scoreDuration start (_hold config)
    (whoFirst,) <$>
        adjustedTransitions config 0 evenTransitions (start + hold, end)

-- | Resolve start and end Directions to the first and second trill notes.
convertDirection :: Bool -> Maybe Direction -> Maybe Direction
    -> (AbsoluteMode, Maybe Bool)
    -- ^ Who starts the trill.  The boolean indicates whether the transitions
    -- should be even to end on the expected end Direction, and Nothing if it
    -- doesn't matter.
convertDirection neighborLow start end = (first, evenTransitions)
    where
    first = case start of
        Nothing -> Unison
        Just Low -> if neighborLow then Neighbor else Unison
        Just High -> if neighborLow then Unison else Neighbor
    -- If I end Low, and neighbor is low, and I started with Unison, then val2
    -- is low, so I want even transitions.  Why is it so complicated just to
    -- get a trill to end high or low?
    firstLow = case first of
        Unison -> not neighborLow
        Neighbor -> neighborLow
    evenTransitions = case end of
        Nothing -> Nothing
        Just Low -> Just (not firstLow)
        Just High -> Just firstLow

-- | Turn transition times into a trill control.
smoothTrill :: ScoreT.Function -- ^ time to take make the transition,
    -- where 0 is instant and 1 is all available time
    -> ControlUtil.Curve
    -> [(RealTime, Signal.Y)]
    -> Derive.Deriver Signal.Control
smoothTrill time curve transitions = do
    srate <- Call.getSrate
    -- I used to optimize sig_function == const 0, but it probably doesn't make
    -- much difference.
    pure $ ControlUtil.smoothRelative curve srate time transitions

-- | Get trill transition times, adjusted for all the various fancy parameters
-- that trills have.
adjustedTransitions :: Config
    -> ScoreTime
    -> Maybe Bool -- ^ emit an even number of transitions, or Nothing for
    -- however many will fit
    -> (ScoreTime, ScoreTime) -> Derive.Deriver [RealTime]
adjustedTransitions config hold even (start, end) = do
    realEnd <- Derive.real end
    addHold . addBias (_bias config)
        . adjustTransitions realEnd (_adjust config)
        . trim
        =<< trillTransitions (start + hold, end)
            (_includeEnd config) (_speed config)
    where
    addHold transitions
        | hold > 0 = (: drop 1 transitions) <$> Derive.real start
        | otherwise = pure transitions
    trim = case even of
        Nothing -> id
        Just even -> if even then takeEven else takeOdd
    takeEven (x:y:zs) = x : y : takeEven zs
    takeEven _ = []
    takeOdd [x, _] = [x]
    takeOdd (x:y:zs) = x : y : takeOdd zs
    takeOdd xs = xs

adjustTransitions :: RealTime -> Adjust -> [RealTime] -> [RealTime]
adjustTransitions _ Shorten ts = ts
adjustTransitions end Stretch ts@(_:_:_) = zipWith (+) offsets ts
    where
    -- (_:_:_) above means both the last and division are safe.
    stretch = max 0 (end - last ts) / fromIntegral (length ts - 1)
    offsets = Lists.range_ 0 stretch
adjustTransitions _ Stretch ts = ts

addBias :: Double -> [RealTime] -> [RealTime]
addBias _ [] = []
addBias bias (t:ts)
    | bias == 0 = t : ts
    | bias > 0 = t : positive (min 1 (RealTime.seconds bias)) ts
    | otherwise = negative (min 1 (RealTime.seconds (abs bias))) (t:ts)
    where
    positive bias (x:y:zs) = Num.scale x y bias : y : positive bias zs
    positive _ xs = xs
    negative bias (x:y:zs) = x : Num.scale x y bias : negative bias zs
    negative _ xs = xs

-- | Make a trill signal from a list of transition times.  It will alternate
-- between values from the given Functions.
trillFromTransitions :: ScoreT.Function -> ScoreT.Function
    -> RealTime -> [RealTime] -> [(RealTime, Signal.Y)]
trillFromTransitions val1 val2 start transitions =
    initial ++ [(x, sig x) | (x, sig) <- zip transitions (cycle [val1, val2])]
    where
    -- Hold might have push the first transition forward, so make a flat
    -- segment for it.
    initial = case transitions of
        x : _ | start < x -> [(start, val1 start)]
        _ -> []

-- | Create trill transition points from a speed.
trillTransitions :: (ScoreTime, ScoreTime) -> Bool
    -> Typecheck.RealTimeFunctionT -> Derive.Deriver [RealTime]
trillTransitions range includeEnd (Typecheck.RealTimeFunctionT ttype speed) =
    case ttype of
        ScoreT.TReal -> realTransitions range includeEnd speed
        ScoreT.TScore -> scoreTransitions range includeEnd speed

realTransitions :: (ScoreTime, ScoreTime) -> Bool -> ScoreT.Function
    -> Derive.Deriver [RealTime]
realTransitions (start, end) includeEnd speed = do
    start <- Derive.real start
    end <- Derive.real end
    fullCycles RealTime.eta end includeEnd <$>
        Speed.realStarts speed start end

scoreTransitions :: (ScoreTime, ScoreTime) -> Bool -> ScoreT.Function
    -> Derive.Deriver [RealTime]
scoreTransitions (start, end) includeEnd speed = do
    allTransitions <- Speed.scoreStarts speed start end
    mapM Derive.real $ fullCycles ScoreTime.eta end includeEnd allTransitions

-- | Given a list of trill transition times, take only ones with a complete
-- duration.  Otherwise a trill can wind up with a short note at the end, which
-- sounds funny.  However it's ok if the note is slightly too short, as tends
-- to happen with floating point.
fullCycles :: (Ord a, Num a) => a -> a -> Bool -> [a] -> [a]
fullCycles eta end includeEnd vals
    | null cycles = take 1 vals
    | otherwise = cycles
    where
    cycles = go vals
    go (x1 : xs) = case xs of
        x2 : _ | x2 <= end + eta -> x1 : go xs
        _ | includeEnd && x1 - eta <= end -> [x1]
        _ -> []
    go [] = []
