-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Calls to deal with an entire ensemble, or miscellaneous instruments.
module Derive.C.Bali.Gong (library, makeCycle, nrukGenerator) where
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.C.Prelude.Trill as Trill
import qualified Derive.Call as Call
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Speed as Speed
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Types as Types

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("cycle", cCycle)
        ]
    , Library.transformers
        [ ("pokok", cPokok)
        , ("J", cJegog)
        , ("C", cCalung)
        , ("nruk", cNruk)
        ]
    ]

module_ :: Module.Module
module_ = "bali" <> "gong"

cPokok :: Derive.Transformer Derive.Note
cPokok = Derive.transformer module_ "pokok" (Tags.inst <> Tags.underInvert)
    "Add a pokok note with 'Derive.Flags.infer_duration' and the same pitch,\
    \ transposed into the appropriate range."
    $ Sig.callt ((,,)
    <$> Sig.defaulted "octave" Call.Down
        "Which to choose if the pitch exists in multiple octaves."
    <*> Sig.requiredEnviron "insts" Sig.Prefixed "Instruments."
    <*> rangeEnv
    ) $ \(octave, insts, range) -> Sub.underInvert $ \args deriver -> do
        note <- pokok (Args.start args) octave insts range
        deriver <> note

pokok :: ScoreTime -> Call.UpDown -> [ScoreT.Instrument] -> Scale.Range
    -> Derive.Deriver Derive.NoteDeriver
pokok start octave insts range = do
    (parsePitch, showPitch, _) <- Call.getPitchFunctions
    fromPitch <- Call.getParsedPitch parsePitch =<< Derive.real start
    toNote <- Derive.require ("can't show pitch: " <> showt fromPitch) $
        showPitch (restrict range octave fromPitch)
    pure $ realizeNote start insts toNote

-- How to choose high and low ding?
-- I should infer it based on the octave of the melody, but also be able to
-- override easily.  But to know the octave of the melody I need the melody
-- itself along with its range, which is not actually written anywhere or
-- even well defined.  So choose low and do manual for now.

-- | Transpose the pitch by octaves until it fits in the range.
-- Assumes the range is at least one octave, and less than two.
restrict :: Scale.Range -> Call.UpDown -> Pitch.Pitch -> Pitch.Pitch
restrict range octave pitch
    | Scale.inRange range pitch = pitch
    | otherwise = case octave of
        Call.Down
            | withOct (octOf bottom) < bottom -> withOct (octOf bottom + 1)
            | otherwise -> withOct (octOf bottom)
            where bottom = Scale.rangeBottom range
        Call.Up
            | withOct (octOf top) > top -> withOct (octOf top - 1)
            | otherwise -> withOct (octOf top)
            where top = Scale.rangeTop range
    where
    withOct oct = pitch { Pitch.pitchOctave = oct }
    octOf = Pitch.pitchOctave

realizeNote :: ScoreTime -> [ScoreT.Instrument] -> Pitch.Note
    -> Derive.NoteDeriver
realizeNote start instruments note =
    Call.addFlags Flags.inferDuration $
        mconcatMap (\inst -> Derive.withInstrument inst realize1) instruments
    where
    realize1 = Derive.at start $
        Call.transposedPitchedNote =<< Call.evalNote start note

rangeEnv :: Sig.Parser Scale.Range
rangeEnv = Scale.Range
    <$> Sig.requiredEnviron "bottom" Sig.Prefixed "Bottom of the range."
    <*> Sig.requiredEnviron "top" Sig.Prefixed "Top of the range."

makePokok :: Text -> Scale.Range -> [ScoreT.Instrument]
    -> Derive.Transformer Derive.Note
makePokok name range defaultInsts = Derive.transformer module_
    (Derive.CallName name) (Tags.inst <> Tags.underInvert)
    ("Add a " <> Doc.literal name <> " note with 'Derive.Flags.infer_duration'\
        \ and the same pitch, transposed into the " <> Doc.literal name
        <> " range.")
    $ Sig.callt ((,)
    <$> Sig.defaulted "octave" Call.Down
        "If the pitch exists in multiple octaves, choose this one."
    <*> Sig.environ "insts" Sig.Prefixed defaultInsts "Instruments."
    ) $ \(octave, insts) -> Sub.underInvert $ \args deriver -> do
        note <- pokok (Args.start args) octave insts range
        deriver <> note

cJegog :: Derive.Transformer Derive.Note
cJegog = makePokok "jegog" (BaliScales.instrumentRange Legong.jegog)
    ["jegog-p", "jegog-s"]

cCalung :: Derive.Transformer Derive.Note
cCalung = makePokok "calung" (BaliScales.instrumentRange Legong.calung)
    ["calung-p", "calung-s"]

-- * nruk tuk

cNruk :: Derive.Transformer Derive.Note
cNruk = Derive.transformer module_ "nruk" Tags.inst
    "Nruktuk, for kajar or gangsa."
    $ Sig.callt nrukArgs nruk

nrukGenerator :: Module.Module -> Derive.CallName -> Doc.Doc
    -> (Derive.NoteArgs -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
nrukGenerator mod name doc deriver = Derive.generator mod name Tags.inst doc $
    Sig.call nrukArgs $ \params args -> nruk params args (deriver args)

nrukArgs :: Sig.Parser (Speed.Speed, Speed.Speed, Signal.Y, DeriveT.Duration)
nrukArgs = (,,,)
    <$> Sig.defaulted "start" (Speed.Real 4) "Start speed."
    <*> Sig.defaulted "end" (Speed.Real 19) "End speed."
    <*> Sig.defaulted "end-dyn" (0.15 :: Double)
        "Dyn multiplier when the stroke duration reaches 0."
    <*> Trill.holdEnv

nruk :: (Speed.Speed, Speed.Speed, Signal.Y, DeriveT.Duration)
    -> Derive.PassedArgs a -> Derive.NoteDeriver -> Derive.NoteDeriver
nruk (startSpeed, endSpeed, endDyn, hold) args deriver = do
    starts <- Trill.tremoloStartsCurve curve hold startSpeed endSpeed
        (Args.rangeOrNext args)
    dyns <- dynFromDuration endDyn <$> mapM Derive.real starts
    realizeNruk (Args.normalized args deriver) (zip starts dyns)
    where
    -- TODO it seems like it should start slower, but
    -- ControlUtil.expon 2 spends too little time fast.
    curve = id

realizeNruk :: Derive.NoteDeriver -> [(ScoreTime, Signal.Y)]
    -> Derive.NoteDeriver
realizeNruk deriver notes = Sub.derive
    [ SubT.EventT start 0 (Call.multiplyDynamic dyn deriver)
    | (start, dyn) <- notes
    ]

-- | Decrease dyn as note duration decreases.  Over a threshold, dyn is 1.
-- Under that it approaches @low_dyn@ as the dur approaches 0.
dynFromDuration :: Signal.Y -> [RealTime] -> [Signal.Y]
dynFromDuration lowDyn = map dynAt . durations
    where
    dynAt dur
        | dur > threshold = 1
        | otherwise = f dur
        where f = ControlUtil.makeFunction id threshold 1 0 lowDyn
    -- Dyn is 1 above this.
    threshold = 0.18

durations :: Num a => [a] -> [a]
durations starts = zipWith (-) (drop 1 starts) starts
    -- This loses the last one, but it's ok because that's the end time.

-- * cycle

cCycle :: Derive.Generator Derive.Note
cCycle = makeCycle "cycle" Nothing Nothing

makeCycle :: Derive.CallName
    -> Maybe (Either Text [DeriveT.Quoted])
    -> Maybe (Either Meter.Rank Typecheck.DefaultScore)
    -> Derive.Generator Score.Event
makeCycle name defaultStrokes defaultDur =
    Derive.generator module_ name Tags.inst
    "Cycle calls. Align to the end for negative duration."
    $ Sig.call ((,)
    <$> Sig.maybeDefaulted "strokes" defaultStrokes "Cycle these strokes."
    <*> Sig.maybeDefaulted "dur" defaultDur "Duration of each stroke.\
        \ A string is taken as a timestep."
    ) $ \(strokes, dur) args -> do
        strokes <- pure $ case strokes of
            Left str -> map (DeriveT.quoted0 . Expr.Symbol . Text.singleton)
                (Text.unpack str)
            Right strs -> strs
        dur <- case dur of
            Left ts -> DeriveT.ScoreDuration <$>
                Call.timestepDuration args ts
            Right (Typecheck.DefaultScore dur) -> pure dur
        dur <- Derive.score dur
        Args.normalizedStart args $
            callCycle (Args.context args) (Args.range args)
                (Args.orientation args) strokes dur

callCycle :: Derive.Context Score.Event -> (TrackTime, TrackTime)
    -> Types.Orientation -> [DeriveT.Quoted]
    -> ScoreTime -> Derive.NoteDeriver
callCycle ctx (start, end) orient calls dur =
    mconcat [Derive.at t $ Eval.evalQuoted ctx call | (t, call) <- ts]
    where
    ts = case orient of
        Types.Positive -> zip (Lists.range' start end dur) (cycle calls)
        Types.Negative -> reverse $
            zip (Lists.range' end start (-dur)) (cycle (reverse calls))
