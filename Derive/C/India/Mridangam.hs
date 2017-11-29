-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MultiWayIf #-}
-- | Library of standard mridangam patterns.
module Derive.C.India.Mridangam where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
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
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Typecheck as Typecheck

import Global
import Types


library :: Derive.Library
library = Library.generators
    [ ("seq", c_sequence sequence_arg)
    , ("p", c_pattern)
    , ("tir", c_tirmanam)

    -- standard sequences
    -- There are various other ways to play this.
    , ("tari", c_sequence (p "n+u+kt+k")) -- naka tiku tari kita
    -- This is the name I use in Solkattu.
    , ("na", c_sequence (p "n+u+kt+k"))
    , ("tk", c_sequence (p "k+"))
    , ("tknk", c_sequence (p "k+n+"))
    ]
    where
    p = pure . parse_sequence

module_ :: Module.Module
module_ = "india" <> "mridangam"

c_sequence :: Sig.Parser [Stroke] -> Derive.Generator Derive.Note
c_sequence sequence_arg = Derive.generator module_ "sequence" Tags.inst
    "Play a sequence of mridangam strokes. Each one takes the given `dur`, and\
    \ if the event is longer than the sequence, it is repeated to fill up\
    \ available space. If a whole cycle doesn't fit, clip from the end for a\
    \ positive event, or from the beginning for a negative one.\
    \ If `dur` is 0, then stretch to the sequence to fit the event."
    $ Sig.call ((,) <$> sequence_arg <*> dur_arg) $ \(sequence, dur) ->
    Sub.inverting $ \args -> do
        let seq = map (1,) $ map (realize_stroke (Args.context args)) sequence
        m_sequence seq dur (Args.range args) (Args.orientation args)

sequence_arg :: Sig.Parser [Stroke]
sequence_arg = parse_sequence <$> Sig.required_env "sequence" Sig.Unprefixed
    "Single letter stroke names.  `_` or space is a rest."

dur_arg :: Sig.Parser ScoreTime
dur_arg = Typecheck.non_negative <$> Sig.defaulted_env "dur" Sig.Both 0
    "Duration for each letter in the sequence. If 0, the sequence will\
    \ stretch to the event's duration."

m_sequence :: [(Sequence.Duration, Maybe Derive.NoteDeriver)] -> ScoreTime
    -> (TrackTime, TrackTime) -> Event.Orientation -> Derive.NoteDeriver
m_sequence notes dur (start, end) orientation = realize $ case orientation of
    _ | dur == 0 -> stretch_to_range (start, end) notes
    Event.Positive -> takeWhile ((<end) . fst) $ place start dur (cycle notes)
    Event.Negative -> reverse $ takeWhile ((>=start) . fst) $
        place end (-dur) $ cycle (reverse notes)
    where
    place from step = Seq.map_maybe_snd id . snd . List.mapAccumL note from
        where note t (d, n) = (t + realToFrac d * step, (t, n))
    realize notes =
        mconcat [Derive.place start 0 note | (start, note) <- notes]

stretch_to_range :: (ScoreTime, ScoreTime) -> [(Sequence.Duration, Maybe a)]
    -> [(ScoreTime, a)]
stretch_to_range (start, end) dur_notes =
    [(t, note) | (t, Just note) <- zip starts notes]
    where
    starts = scanl (+) start $ map ((*factor) . realToFrac) durs
    (durs, notes) = unzip dur_notes
    factor = (end - start) / realToFrac (sum durs)

-- TODO make this into a Typecheck
-- actually I think I maybe don't support that?
parse_sequence :: Text -> [Stroke]
parse_sequence = map parse . Text.unpack
    where
    parse c
        | c == ' ' || c == '_' = Rest
        | otherwise = Stroke c

data Stroke = Rest | Stroke Char
    deriving (Eq, Show)

instance Pretty Stroke where pretty = ShowVal.show_val
instance ShowVal.ShowVal Stroke where
    show_val Rest = "_"
    show_val (Stroke c) = Text.singleton c

-- * c_pattern

c_pattern :: Derive.Generator Derive.Note
c_pattern = Derive.generator module_ "pattern" Tags.inst
    "Like `seq`, but pick a standard pattern."
    $ Sig.call ((,,)
    <$> (fmap Typecheck.positive
        <$> Sig.required "n" "Number of strokes. If not given, and dur > 0,\
            \ then infer the number of strokes as the event_duration / dur.")
    <*> Sig.defaulted_env "var" Sig.Both default_variation
        ("Variation name. Possibilities are: "
            <> Doc.commas (map Doc.literal (Map.keys variations)))
    <*> dur_arg
    ) $ \(maybe_strokes, variation, dur) -> Sub.inverting $ \args -> do
        strokes <- maybe (infer_strokes dur (Args.duration args)) return
            maybe_strokes
        notes <- Derive.require_right id $ infer_pattern strokes variation
        notes <- return $
            map (second (realize_mstroke (Args.context args))) notes
        m_sequence notes dur (Args.range args) (Args.orientation args)

infer_pattern :: Sequence.Matra -> Text
    -> Either Text [(Sequence.Duration, Realize.Note Mridangam.Stroke)]
infer_pattern dur variation = do
    patterns <- justErr ("unknown variation " <> showt variation) $
        Map.lookup variation variations
    notes <- justErr
        ("variation " <> showt variation <> " doesn't have duration: "
            <> showt dur)
        (Realize.lookup_pattern (Solkattu.PatternM dur) patterns)
    -- (*4) because each note is 1 matra, which is 1/4 Duration, and I want
    -- duration in matras.
    return $ map (first (*4)) $ Sequence.tempo_to_duration $
        map (first Sequence._tempo) $ Sequence.flatten notes

realize_mstroke :: Derive.Context Score.Event -> Realize.Note Mridangam.Stroke
    -> Maybe Derive.NoteDeriver
realize_mstroke ctx = fmap (Eval.eval_expr_text ctx) . stroke_expr

stroke_expr :: Realize.Note Mridangam.Stroke -> Maybe (Expr.Expr Text)
stroke_expr stroke = case stroke of
    Realize.Note stroke -> Just $ Expr.to_expr stroke
    Realize.Space {} -> Nothing
    Realize.Pattern p -> Just $ Expr.to_expr p

infer_strokes :: ScoreTime -> ScoreTime -> Derive.Deriver Int
infer_strokes dur event_dur
    | dur > 0 = return $ floor (event_dur / dur)
    | otherwise = Derive.throw "can't infer both number of strokes and\
        \ duration of strokes simultaneously"

default_variation :: Text
default_variation = "d"

variations :: Map Text Mridangam.Patterns
variations = Map.fromList $
    [ (default_variation, Mridangam.default_patterns)
    , ("kt_kn_o", Mridangam.kt_kn_o)
    ] ++
    [ ("f567-" <> showt n, p) | (n, p) <- zip [0..] Mridangam.families567]

-- * c_tirmanam

c_tirmanam :: Derive.Generator Derive.Note
c_tirmanam = Derive.with_score_duration score_duration $
    Derive.generator module_ "tir" Tags.inst
    "Repeat a sequence three times. If the duration is negative, put the first\
    \ stroke of the karvai at the end time with `{strong}`."
    $ Sig.call signature $ \(sequence, karvai, matra_dur) ->
    Sub.inverting $ \args -> do
        sequence3 <- Derive.require_right id $
            tirmanam sequence karvai matra_dur (Args.duration args)
        realize_sequence (Args.context args) (Args.range args) sequence3
    where
    signature = (,,)
        <$> sequence_arg
        <*> (parse_sequence <$> Sig.defaulted "karvai" ""
        "Separates each sequence. If it's empty or a single non-rest, then the\
        \ gap can stretch to an integral number of matras.")
        <*> dur_arg -- TODO nadai arg?
    score_duration args = do
        (sequence, karvai, matra_dur) <- Sig.parse_or_throw signature args
        return $ Derive.CallDuration $ if
            | matra_dur == 0 -> Args.duration args
            | otherwise -> (if Args.negative args then negate else id) $
                fromIntegral (length sequence * 3 + length karvai * 2)
                    * matra_dur

realize_sequence :: Derive.Context Score.Event -> (ScoreTime, ScoreTime)
    -> [(ScoreTime, Stroke)] -> Derive.NoteDeriver
realize_sequence ctx (start, end) dur_strokes = mconcat
    [Derive.place t 0 (add_flag t note) | (t, Just note) <- zip starts notes]
    where
    add_flag t note
        | t == end = Call.add_flags Flags.strong note
        | otherwise = note
    notes = map (realize_stroke ctx) strokes
    (durs, strokes) = unzip dur_strokes
    starts = scanl (+) start durs

tirmanam :: [Stroke] -> [Stroke] -> ScoreTime -> ScoreTime
    -> Either Text [(ScoreTime, Stroke)]
tirmanam sequence karvai matra_dur event_dur = (add_final=<<) $ if
    | matra_dur == 0 ->
        let p = sequence ++ karvai ++ sequence ++ karvai ++ sequence
        in return $ map (abs event_dur / fromIntegral (length p),) p
    | otherwise -> do
        karvai_durs <- first (("event dur " <> pretty event_dur <> ": ")<>) $
            stretch_karvai sequence karvai matra_dur (abs event_dur)
        let p = map (matra_dur,) sequence
        return $ p ++ karvai_durs ++ p ++ karvai_durs ++ p
    where
    add_final sequence
        | event_dur < 0 = case Seq.head karvai of
            Just s | s /= Rest -> return $ sequence ++ [(0, s)]
            _ -> Left "karvai should start with non-rest for a negative event"
        | otherwise = return sequence

realize_stroke :: Derive.Context Score.Event -> Stroke
    -> Maybe Derive.NoteDeriver
realize_stroke _ Rest = Nothing
realize_stroke ctx (Stroke c) = Just $ do
    call <- Eval.get_generator (Expr.Symbol (Text.singleton c))
    Eval.apply_generator ctx call []

stretch_karvai :: [Stroke] -> [Stroke] -> ScoreTime -> ScoreTime
    -> Either Text [(ScoreTime, Stroke)]
stretch_karvai sequence karvai matra_dur event_dur = if
    | matra_dur == 0 -> Left "matra dur of 0" -- caller should stretch
    | stretch -> if
        | stretch_dur < 0 -> Left $ "would have to stretch karvai to "
            <> pretty stretch_dur
        | not (Num.integral (karvai_dur / matra_dur)) ->
            Left $ "karvai would have to be " <> pretty (karvai_dur / matra_dur)
                <> " matras"
        | otherwise -> case Seq.viewr karvai of
            Nothing -> Right [(karvai_dur, Rest)]
            Just (ks, k) ->
                Right $ map (matra_dur,) ks ++ [(stretch_dur, k)]
    | to_dur strokes /= event_dur ->
        Left $ "expected " <> pretty strokes <> "*" <> pretty matra_dur
            <> " = " <> pretty (to_dur strokes)
    | otherwise -> Right $ map (matra_dur,) karvai
    where
    strokes = length sequence * 3 + length karvai * 2
    -- Total duration of one karvai.
    karvai_dur = (event_dur - to_dur (length sequence * 3)) / 2
    -- The final karvai stroke must be this duration.
    stretch_dur = karvai_dur - to_dur (max 0 (length karvai - 1))
    stretch = case karvai of
        [] -> True
        [s] | s /= Rest -> True
        _ -> False
    to_dur = (*matra_dur) . fromIntegral
