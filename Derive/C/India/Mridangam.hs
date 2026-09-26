-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of standard mridangam patterns.
module Derive.C.India.Mridangam where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Instrument.ToScore as ToScore
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import qualified Ui.Types as Types

import           Global
import           Types


library :: Library.Library
library = Library.generators
    [ ("seq", cSequence sequenceArg)
    , ("pat", cPattern)
    , ("tir", cTirmanam)

    -- standard sequences
    -- nakatarikitataka - dikutarikitataka is also standard: tpupktpk
    , ("8n", cSequence (p "npupktpk"))
    , ("tk", cSequence (p "kp"))
    , ("tknk", cSequence (p "kpnp"))
    ]
    where
    p = pure . parseSequence

module_ :: Module.Module
module_ = "india" <> "mridangam"

cSequence :: Sig.Parser [Stroke] -> Derive.Generator Derive.Note
cSequence sequenceArg = Derive.withScoreDuration scoreDuration $
    Derive.generator module_ "sequence" Tags.inst
    "Play a sequence of mridangam strokes. Each one takes the given `dur`, and\
    \ if the event is longer than the sequence, it is repeated to fill up\
    \ available space. If a whole cycle doesn't fit, clip from the end for a\
    \ positive event, or from the beginning for a negative one.\
    \ If `dur` is 0, then stretch to the sequence to fit the event."
    $ Sig.call signature $ \(sequence, dur) ->
    Sub.inverting $ \args -> do
        let seq = map (1,) $ map (realizeStroke (Args.context args)) sequence
        mSequence seq dur (Args.range args) (Args.orientation args)
    where
    signature = (,) <$> sequenceArg <*> durArg
    scoreDuration args = do
        (sequence, matraDur) <- Sig.parseOrThrow signature args
        pure $ Derive.CallDuration $ if
            | matraDur == 0 -> Args.duration args
            | otherwise -> (if Args.negative args then negate else id) $
                fromIntegral (length sequence) * matraDur

sequenceArg :: Sig.Parser [Stroke]
sequenceArg = parseSequence <$> Sig.requiredEnv "sequence" Sig.Unprefixed
    "Single letter stroke names.  `_` or space is a rest."

durArg :: Sig.Parser ScoreTime
durArg = Typecheck.nonNegative <$>
    Sig.defaultedEnv "dur" Sig.Both (0 :: ScoreTime)
        "Duration for each letter in the sequence. If 0, the sequence will\
        \ stretch to the event's duration."

mSequence :: [(S.Duration, Maybe Derive.NoteDeriver)] -> ScoreTime
    -> (TrackTime, TrackTime) -> Types.Orientation -> Derive.NoteDeriver
mSequence notes dur (start, end) orientation = realize $ case orientation of
    _ | dur == 0 -> stretchToRange (start, end) notes
    Types.Positive -> takeWhile ((<end) . fst) $ place start dur (cycle notes)
    Types.Negative -> reverse $ takeWhile ((>=start) . fst) $
        place end (-dur) $ cycle (reverse notes)
    where
    place from step = Lists.mapMaybeSnd id . snd . List.mapAccumL note from
        where note t (d, n) = (t + realToFrac d * step, (t, n))
    realize notes =
        mconcat [Derive.place start 0 note | (start, note) <- notes]

stretchToRange :: (ScoreTime, ScoreTime) -> [(S.Duration, Maybe a)]
    -> [(ScoreTime, a)]
stretchToRange (start, end) durNotes =
    [(t, note) | (t, Just note) <- zip starts notes]
    where
    starts = scanl (+) start $ map ((*factor) . realToFrac) durs
    (durs, notes) = unzip durNotes
    factor = (end - start) / realToFrac (Num.sum durs)

parseSequence :: Text -> [Stroke]
parseSequence = map parse . Text.unpack
    where
    parse c
        | c == ' ' || c == '_' = Rest
        | otherwise = Stroke c

data Stroke = Rest | Stroke Char
    deriving (Eq, Show)

instance Pretty Stroke where pretty = ShowVal.showVal
instance ShowVal.ShowVal Stroke where
    showVal Rest = "_"
    showVal (Stroke c) = Text.singleton c

-- * c_pattern

cPattern :: Derive.Generator Derive.Note
cPattern = Derive.withScoreDuration scoreDuration $
    Derive.generator module_ "pattern" Tags.inst
    "Like `seq`, but pick a standard pattern."
    $ Sig.call signature $ \(maybeStrokes, variation, matraDur) ->
    Sub.inverting $ \args -> do
        strokes <- maybe (inferStrokes matraDur (Args.duration args)) pure
            maybeStrokes
        notes <- Derive.requireRight id $ inferPattern strokes variation
        notes <- pure $
            map (second (realizeMstroke (Args.context args))) notes
        mSequence notes matraDur (Args.range args) (Args.orientation args)
    where
    signature = (,,)
        <$> (fmap Typecheck.positive
            <$> Sig.required "n" "Number of strokes. If not given, and\
                \ dur > 0, then infer the number of strokes as the\
                \ event_duration / dur.")
        <*> Sig.defaultedEnv "var" Sig.Both defaultVariation
            ("Variation name. Possibilities are: "
                <> Doc.commas (map Doc.literal (Map.keys variations)))
        <*> durArg
    scoreDuration args = do
        (maybeStrokes, _, matraDur) <- Sig.parseOrThrow signature args
        pure $ Derive.CallDuration $ case maybeStrokes of
            Nothing -> Args.duration args
            Just strokes -> fromIntegral strokes * matraDur

inferPattern :: S.Matra -> Text
    -> Either Text [(S.Duration, Realize.Note Mridangam.Stroke)]
inferPattern dur variation = do
    patterns <- justErr ("unknown variation " <> showt variation) $
        Map.lookup variation variations
    notes <- justErr
        ("variation " <> showt variation <> " doesn't have duration: "
            <> showt dur)
        (Realize.lookupPattern (Solkattu.pattern dur) patterns)
    -- (*4) because each note is 1 matra, which is 1/4 Duration, and I want
    -- duration in matras.
    pure $ map (first (*4)) $ S.flattenedNotes $
        S.withDurations $ S.flatten notes

realizeMstroke :: Derive.Context Score.Event -> Realize.Note Mridangam.Stroke
    -> Maybe Derive.NoteDeriver
realizeMstroke ctx = fmap (Eval.evalExprVal ctx) . ToScore.toExpr

inferStrokes :: ScoreTime -> ScoreTime -> Derive.Deriver Int
inferStrokes dur eventDur
    | dur > 0 = pure $ floor (eventDur / dur)
    | otherwise = Derive.throw "can't infer both number of strokes and\
        \ duration of strokes simultaneously"

defaultVariation :: Text
defaultVariation = "d"

variations :: Map Text (Realize.PatternMap Mridangam.Stroke)
variations = Map.fromList $
    [ (defaultVariation,  Mridangam.defaultPatterns)
    , ("kt_kn_o", Mridangam.kt_kn_o)
    ] ++
    [ ("f567-" <> showt n, p) | (n, p) <- zip [0..] Mridangam.families567]

-- * c_tirmanam

cTirmanam :: Derive.Generator Derive.Note
cTirmanam = Derive.withScoreDuration scoreDuration $
    Derive.generator module_ "tir" Tags.inst
    "Repeat a sequence three times. If the duration is negative, put the first\
    \ stroke of the karvai at the end time with `{strong}`."
    $ Sig.call signature $ \(sequence, karvai, matraDur) ->
    Sub.inverting $ \args -> do
        sequence3 <- Derive.requireRight id $
            tirmanam sequence karvai matraDur (Args.duration args)
        realizeSequence (Args.context args) (Args.range args) sequence3
    where
    signature = (,,)
        <$> sequenceArg
        <*> (parseSequence <$> Sig.defaulted "karvai" ("" :: Text)
        "Separates each sequence. If it's empty or a single non-rest, then the\
        \ gap can stretch to an integral number of matras.")
        <*> durArg -- TODO nadai arg?
    scoreDuration args = do
        (sequence, karvai, matraDur) <- Sig.parseOrThrow signature args
        pure $ Derive.CallDuration $ if
            | matraDur == 0 -> Args.duration args
            | otherwise -> (if Args.negative args then negate else id) $
                fromIntegral (length sequence * 3 + length karvai * 2)
                    * matraDur

realizeSequence :: Derive.Context Score.Event -> (ScoreTime, ScoreTime)
    -> [(ScoreTime, Stroke)] -> Derive.NoteDeriver
realizeSequence ctx (start, end) durStrokes = mconcat
    [Derive.place t 0 (addFlag t note) | (t, Just note) <- zip starts notes]
    where
    addFlag t note
        | t == end = Call.addFlags Flags.strong note
        | otherwise = note
    notes = map (realizeStroke ctx) strokes
    (durs, strokes) = unzip durStrokes
    starts = scanl (+) start durs

tirmanam :: [Stroke] -> [Stroke] -> ScoreTime -> ScoreTime
    -> Either Text [(ScoreTime, Stroke)]
tirmanam sequence karvai matraDur eventDur = (addFinal=<<) $ if
    | matraDur == 0 ->
        let p = sequence ++ karvai ++ sequence ++ karvai ++ sequence
        in pure $ map (abs eventDur / fromIntegral (length p),) p
    | otherwise -> do
        karvaiDurs <- first (("event dur " <> pretty eventDur <> ": ")<>) $
            stretchKarvai sequence karvai matraDur (abs eventDur)
        let p = map (matraDur,) sequence
        pure $ p ++ karvaiDurs ++ p ++ karvaiDurs ++ p
    where
    addFinal sequence
        | eventDur < 0 = case Lists.head karvai of
            Just s | s /= Rest -> pure $ sequence ++ [(0, s)]
            _ -> Left "karvai should start with non-rest for a negative event"
        | otherwise = pure sequence

realizeStroke :: Derive.Context Score.Event -> Stroke
    -> Maybe Derive.NoteDeriver
realizeStroke _ Rest = Nothing
realizeStroke ctx (Stroke c) = Just $ do
    call <- Eval.getGenerator (Expr.Symbol (Text.singleton c))
    Eval.applyGenerator ctx call []

stretchKarvai :: [Stroke] -> [Stroke] -> ScoreTime -> ScoreTime
    -> Either Text [(ScoreTime, Stroke)]
stretchKarvai sequence karvai matraDur eventDur = if
    | matraDur == 0 -> Left "matra dur of 0" -- caller should stretch
    | stretch -> if
        | stretchDur < 0 -> Left $ "would have to stretch karvai to "
            <> pretty stretchDur
        | not (Num.integral (karvaiDur / matraDur)) ->
            Left $ "karvai would have to be " <> pretty (karvaiDur / matraDur)
                <> " matras"
        | otherwise -> case Lists.unsnoc karvai of
            Nothing -> Right [(karvaiDur, Rest)]
            Just (ks, k) ->
                Right $ map (matraDur,) ks ++ [(stretchDur, k)]
    | ApproxEq.neq 0.001 (toDur strokes) eventDur ->
        Left $ "expected " <> pretty strokes <> "*" <> pretty matraDur
            <> " = " <> pretty (toDur strokes)
    | otherwise -> Right $ map (matraDur,) karvai
    where
    strokes = length sequence * 3 + length karvai * 2
    -- Total duration of one karvai.
    karvaiDur = (eventDur - toDur (length sequence * 3)) / 2
    -- The final karvai stroke must be this duration.
    stretchDur = karvaiDur - toDur (max 0 (length karvai - 1))
    stretch = case karvai of
        [] -> True
        [s] | s /= Rest -> True
        _ -> False
    toDur = (*matraDur) . fromIntegral
