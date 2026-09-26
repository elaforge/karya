-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Create val calls for scale degrees.  These are the calls that a scale
-- brings into scope, so they should be referenced from
-- 'Derive.scale_note_to_call' implementations.
module Derive.Call.ScaleDegree (
    -- * equal tempered
    scaleDegree, pitchExpr
    -- * just
    , NamedIntervals
    , scaleDegreeJust, scaleDegreeInterval
    , intervalArgDoc, resolveIntervals
) where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch

import           Global


-- * equal tempered

-- | Create a pitch val call for the given scale degree.  This is intended to
-- be used by scales to generate their calls, but of course each scale may
-- define calls in its own way.
scaleDegree :: PSignal.Scale -> Scale.PitchNn -> Scale.PitchNote
    -> Derive.ValCall
scaleDegree scale pitchNn pitchNote = Derive.valCall Module.scale
    "pitch" mempty "Emit the pitch of a scale degree." $
    Sig.call (Sig.defaulted "frac" (0 :: Double)
        "Add this many hundredths of a scale degree to the output.")
    $ \frac _args -> do
        env <- Derive.getEnviron
        let config = PSignal.PitchConfig env controls
            controls = if frac == 0 then mempty
                else Map.singleton Controls.chromatic (frac / 100)
        pure $! PSignal.pitch scale
            (\config -> addAbsoluteTransposers config <$> pitchNn config)
            pitchNote config

-- | Apply transpose signals that don't require any scale knowledge.
addAbsoluteTransposers :: PSignal.PitchConfig -> Pitch.NoteNumber
    -> Pitch.NoteNumber
addAbsoluteTransposers config nn =
    Pitch.addHz (Map.findWithDefault 0 Controls.hz controls)
        (nn + Pitch.nn (Map.findWithDefault 0 Controls.nn controls))
    where controls = PSignal.pitchControls config

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
--
-- TODO This is actually totally wrong, because a Pitch.Note isn't expected an
-- expression, but just the call part of the expression.  Other functions want
-- to parse it, and they're not expecting spaces in there.  What this should
-- actually do is return a real @Expr Text@, or a specialized version where
-- the Call is known to be a Pitch.Note.  But for that, scale_input_to_note
-- would have to change, and that would probably touch a lot of things.  So
-- for the moment, I leave this function in place to document where a
-- hypothetical pitch_expr should be called, but it doesn't actually do
-- anything.
pitchExpr :: Double -> Pitch.Note -> Pitch.Note
pitchExpr _frac note = note

_pitchExpr :: Double -> Pitch.Note -> Expr.Expr Expr.MiniVal
_pitchExpr frac note = Expr.generator $ Expr.call (noteSymbol note) $
    if frac == 0 then []
    else [Expr.toVal (floor (frac * 100) :: Int)]

noteSymbol :: Pitch.Note -> Expr.Symbol
noteSymbol note = Expr.Symbol (Pitch.noteText note)

-- * just

-- | Map from named intervals to the interval's ratio.
type NamedIntervals = Map Text Ratio.Rational

-- | A fancier version of 'scale_degree' that takes interval arguments.
scaleDegreeJust :: PSignal.Scale -> NamedIntervals
    -> Pitch.Hz -- ^ add an arbitrary extra interval to the output
    -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scaleDegreeJust scale namedIntervals extraInterval pitchNn pitchNote =
    Derive.valCall Module.scale "pitch" mempty
    "Emit the pitch of a scale degree."
    $ Sig.call (Sig.many "interval" (intervalArgDoc namedIntervals))
    $ \intervals _ -> do
        interval <- resolveIntervals namedIntervals intervals
        env <- Derive.getEnviron
        pure $! PSignal.pitch scale
            (\config -> modify (extraInterval*interval) config <$>
                pitchNn config)
            pitchNote (PSignal.PitchConfig env mempty)
    where
    modify interval config = addAbsoluteTransposers config
        . Pitch.modifyHz (*interval)

scaleDegreeInterval :: PSignal.Scale -> NamedIntervals -> Pitch.Note
    -> Maybe Derive.ValCall
scaleDegreeInterval scale namedIntervals note =
    case parseRelativeInterval namedIntervals note of
        Nothing -> Nothing
        Just interval ->
            Just $ relativeScaleDegree scale namedIntervals interval

intervalArgDoc :: NamedIntervals -> Doc.Doc
intervalArgDoc namedIntervals =
    "Multiply this interval with the note's frequency. Negative numbers\
    \ divide, so while `3/2` goes up a fifth, `-3/2` goes down a fifth.\
    \ Can be either a ratio or a symbol drawn from: "
    <> Doc.commas (map Doc.literal (Map.keys namedIntervals))

parseRelativeInterval :: NamedIntervals -> Pitch.Note -> Maybe Pitch.Hz
parseRelativeInterval namedIntervals note =
    unsign <$> (resolveInterval namedIntervals (Pitch.noteText note)
        <|> parseNum)
    where
    parseNum = case Parse.parseVal (Pitch.noteText note) of
        Right val
            | Just (ScoreT.Typed ScoreT.Untyped num)
                <- DeriveT.constantVal val -> Just num
        _ -> Nothing
    unsign val = if val < 0 then recip (abs val) else val

relativeScaleDegree :: PSignal.Scale -> NamedIntervals -> Pitch.Hz
    -> Derive.ValCall
relativeScaleDegree scale namedIntervals initialInterval =
    Derive.valCall Module.scale "pitch" mempty
    "Emit a pitch that is a relative interval from the previous pitch." $
    Sig.call (Sig.many "interval" (intervalArgDoc namedIntervals))
    $ \intervals args -> do
        interval <- (initialInterval*) <$>
            resolveIntervals namedIntervals intervals
        start <- Args.realStart args
        Derive.require "relative interval requires a previous pitch" $ do
            Derive.TagPitch prev <- Args.prevVal args
            Pitches.modifyHz scale (*interval) <$> PSignal.at prev start

resolveIntervals :: NamedIntervals -> [Either Pitch.Hz Text]
    -> Derive.Deriver Pitch.Hz
resolveIntervals namedIntervals intervals =
    product . map unsign <$> mapM (either pure resolve) intervals
    where
    resolve text = Derive.require ("unknown named interval: " <> showt text) $
        resolveInterval namedIntervals text
    unsign val = if val < 0 then recip (abs val) else val

resolveInterval :: NamedIntervals -> Text -> Maybe Pitch.Hz
resolveInterval namedIntervals text = case Text.uncons text of
    Just ('-', text) -> negate <$> lookup text
    _ -> lookup text
    where lookup text = realToFrac <$> Map.lookup text namedIntervals
