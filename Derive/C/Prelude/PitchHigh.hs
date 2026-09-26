-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of basic high level pitch calls.
--
-- High level calls do something a little more abstract and \"musical\"
-- than the low level calls in "Derive.Call.Prelude.Pitch".  Generally they
-- have complete-word names, while low level calls are just single letters.
--
-- TODO this module has a dumb name.  What would be better?
module Derive.C.Prelude.PitchHigh (library) where
import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Pitch as Call.Pitch
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.PitchUtil as PitchUtil
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import           Derive.Sig (defaulted)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Types


library :: Library.Library
library = mconcat
    [ Library.transformers
        [ ("lift", cLiftNote)
        , ("drop", cDropNote)
        , ("Lift", cLiftNoteStart)
        , ("Drop", cDropNoteStart)
        ]
    , Library.generators
        [ ("drop", cDrop)
        , ("lift", cLift)
        , ("ad", cApproachDyn)
        ]
    ]

-- * note calls

cLiftNote :: Derive.Transformer Derive.Note
cLiftNote = makeNoteFade "lift"
    "Raise pitch and drop `dyn` at the end of the note. Same as the `drop`\
    \ note call, except it defaults to going up instead of down."
    PitchLift AlignEnd AlignStart

cDropNote :: Derive.Transformer Derive.Note
cDropNote = makeNoteFade "drop"
    "Drop pitch and `dyn` at the end of the note."
    PitchDrop AlignEnd AlignStart

cLiftNoteStart :: Derive.Transformer Derive.Note
cLiftNoteStart = makeNoteFade "Lift"
    "Attack the note from a lower neighbor.  This is like the `n` pitch call,\
    \ but it's a note call, and it fades in `dyn` at the same time."
    PitchDrop AlignStart AlignEnd
    -- The PitchDirection is reversed because a lift at the beginning of a note
    -- comes from below.  Also, I align the pitch to the end of the fade
    -- for symmetry with 'drop' and 'lift', I'm not sure if it's actually more
    -- musically useful that way.

cDropNoteStart :: Derive.Transformer Derive.Note
cDropNoteStart = makeNoteFade "Drop"
    "Like `Lift`, but attack the note from a higher neighbor."
    PitchLift AlignStart AlignEnd
    -- Like 'c_lift_note_start', the PitchDirection is reversed.

makeNoteFade :: Derive.CallName -> Doc.Doc -> PitchDirection -> Align
    -> Align -> Derive.Transformer Derive.Note
makeNoteFade name doc pitchDir align alignFade =
    Derive.transformer Module.prelude name Tags.underInvert doc
    $ Sig.callt fadeArgs
    $ \(interval, Typecheck.DefaultReal time, maybeFade, curve) ->
    Sub.underInvert $ \args deriver -> do
        let fade = case maybeFade of
                Nothing -> time
                Just (Typecheck.DefaultReal t) -> t
        ranges@((pitchStart, _), _) <- pitchFadeRanges align alignFade
            fade time (Args.start args) (Args.end args)
        Derive.pitchAt pitchStart >>= \case
            Nothing -> deriver
            Just pitch -> do
                (slide, dyn) <-
                    pitchFade align curve pitch pitchDir interval ranges
                pitchSig <- Internal.getDynamic Derive.statePitch
                let merged = case align of
                        -- Since the initial slide has to override the base
                        -- pitch, I can't just merge normally.
                        AlignStart -> PSignal.prepend slide pitchSig
                        AlignEnd -> pitchSig <> slide
                Call.multiplyControl Controls.dynamic (ScoreT.untyped dyn) $
                    Derive.withPitch merged deriver

fadeArgs :: Sig.Parser (Either Pitch.Transpose PSignal.Pitch,
    Typecheck.DefaultReal, Maybe Typecheck.DefaultReal, ControlUtil.Curve)
fadeArgs = (,,,)
    <$> defaulted "interval"
        (Left (Pitch.Chromatic 7) :: Either Pitch.Transpose Sig.Dummy)
        "Interval or destination pitch."
    <*> defaulted "time" (Typecheck.real 0.25) "Time to the destination pitch."
    <*> defaulted "fade" (Nothing :: Maybe Sig.Dummy)
        "Time to fade from or to nothing. If the fade is longer than the pitch\
        \ time, the pitch will finish moving before the dyn has faded out."
    <*> ControlUtil.curveEnv

-- * pitch calls

cDrop :: Derive.Generator Derive.Pitch
cDrop = makePitchFade "drop" "Drop pitch and `dyn`." PitchDrop

cLift :: Derive.Generator Derive.Pitch
cLift = makePitchFade "lift"
    "Lift pitch and drop `dyn`. This is the same as `drop`, except that it\
    \ defaults to going up instead of down."
    PitchLift

makePitchFade :: Derive.CallName -> Doc.Doc -> PitchDirection
    -> Derive.Generator Derive.Pitch
makePitchFade name doc pitchDir =
    Derive.generator1 Module.prelude name Tags.cmod doc
    $ Sig.call fadeArgs
    $ \(interval, Typecheck.DefaultReal time, maybeFade, curve) args -> do
        let fade = case maybeFade of
                Nothing -> time
                Just (Typecheck.DefaultReal t) -> t
        case Args.prevPitch args of
            Nothing -> pure mempty
            Just (_, prevPitch) -> do
                (slide, dyn) <- pitchFade AlignEnd curve prevPitch pitchDir
                    interval =<< pitchFadeRanges AlignStart AlignStart
                        fade time (Args.start args) (Args.start args)
                next <- Derive.real (Args.next args)
                ControlUtil.multiplyDyn next dyn
                pure slide

cApproachDyn :: Derive.Generator Derive.Pitch
cApproachDyn = Derive.generator1 Module.prelude "approach-dyn"
    (Tags.cmod <> Tags.next)
    "Like `approach`, slide to the next pitch, but also drop the `dyn`."
    $ Sig.call ((,,)
    <$> defaulted "time" (Typecheck.real 0.2)
        "Time to get to destination pitch and dyn."
    <*> defaulted "dyn" (0.25 :: Double) "Drop `dyn` by this factor."
    <*> ControlUtil.curveEnv
    ) $ \(Typecheck.DefaultReal time, dyn, curve) args -> do
        (start, end) <- Call.durationFromStart args time
        ControlUtil.multiplyDyn end
            =<< ControlUtil.makeSegment ControlUtil.Linear start 1 end dyn
        Call.Pitch.approach args curve start end

-- * fade implementation

data Align = AlignStart | AlignEnd deriving (Show)
data PitchDirection = PitchDrop | PitchLift deriving (Show)

pitchFade :: Align -> ControlUtil.Curve -> PSignal.Pitch -> PitchDirection
    -> Either Pitch.Transpose PSignal.Pitch
    -> ((RealTime, RealTime), (RealTime, RealTime))
    -> Derive.Deriver (PSignal.PSignal, Signal.Control)
pitchFade align curve pitch pitchDir interval
        ((pitchStart, pitchEnd), (fadeStart, fadeEnd)) =
    (,) <$> pitchSegment align curve (min pitchStart fadeStart) pitchStart
                pitchEnd pitch interval pitchDir
        <*> segment ControlUtil.Linear fadeStart dyn1 fadeEnd dyn2
    where
    (dyn1, dyn2) = case align of
        AlignStart -> (0, 1)
        AlignEnd -> (1, 0)

-- | Create envelope start and end times for pitch and fade as follows:
--
-- @
--     --------     align   align_fade
--     ffff         Start   Start
--     pp-->
--     ffff         Start   End
--     <-pp
--         ffff     End     Start
--        <pp--
--         ffff     End     End
--        <--pp
-- @
pitchFadeRanges :: Align -> Align
    -> DeriveT.Duration -> DeriveT.Duration
    -> ScoreTime -> ScoreTime
    -> Derive.Deriver ((RealTime, RealTime), (RealTime, RealTime))
pitchFadeRanges align alignFade fadeTime pitchTime start end = do
    let durFrom = case align of
            AlignStart -> start
            AlignEnd -> end
    fadeTime <- Call.realDuration durFrom fadeTime
    pitchTime <- Call.realDuration durFrom pitchTime
    (fadeStart, fadeEnd) <- case align of
        AlignStart -> (\p -> (p, p + fadeTime)) <$> Derive.real start
        AlignEnd -> (\p -> (p - fadeTime, p)) <$> Derive.real end
    let (pitchStart, pitchEnd) = case alignFade of
            AlignStart ->
                -- Since there's no point doing anything after the fade is
                -- done, extend backwards if pitch is longer than fade.
                min (fadeStart, fadeStart + pitchTime)
                    (fadeEnd - pitchTime, fadeEnd)
            AlignEnd ->
                max (fadeEnd - pitchTime, fadeEnd)
                    (fadeStart, fadeStart + pitchTime)
    pure ((pitchStart, pitchEnd), (fadeStart, fadeEnd))

segment :: ControlUtil.Curve -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
segment curve x1 y1 x2 y2 = do
    sig <- ControlUtil.makeSegment curve x1 y1 x2 y2
    -- TODO why do I need this leading sample?
    pure $ Signal.fromSample 0 y1 <> sig

pitchSegment :: Align -> ControlUtil.Curve
    -> RealTime -- ^ start pitch at this time
    -> RealTime -- ^ start segment
    -> RealTime -- ^ end segment
    -> PSignal.Pitch
    -> Either Pitch.Transpose PSignal.Pitch -> PitchDirection
    -> Derive.Deriver PSignal.PSignal
pitchSegment align curve start0 start end pitch interval pitchDir =
    case align of
        -- If the pitch segment is at the start of the note, then I may need to
        -- override its base pitch with a flat segment.
        AlignStart -> (initial dest <>) <$>
            PitchUtil.makeSegment curve start dest end pitch
        AlignEnd -> PitchUtil.makeSegment curve start pitch end dest
    where
    initial p = PSignal.fromSample start0 p
    dest = case interval of
        Left degrees -> Pitches.transpose (negateInterval degrees) pitch
        Right p -> p
    negateInterval = case pitchDir of
        PitchDrop -> Pitch.modifyTranspose negate
        PitchLift -> id
