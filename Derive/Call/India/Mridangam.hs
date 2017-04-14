-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of standard mridangam patterns.
module Derive.Call.India.Mridangam where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Typecheck as Typecheck

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("seq", c_sequence pattern_arg)
    , ("p", c_pattern)

    -- standard patterns
    -- There are various other ways to play this.
    , ("tari", c_sequence (pure "n+u+kt+k")) -- naka tiku tari kita
    , ("tk", c_sequence (pure "k+"))
    , ("tknk", c_sequence (pure "k+n+"))
    ]

val_calls :: [Derive.LookupCall Derive.ValCall]
val_calls = Derive.call_map
    [ ("infer", c_infer_pattern)
    ]

module_ :: Module.Module
module_ = "india" <> "mridangam"

c_sequence :: Sig.Parser Text -> Derive.Generator Derive.Note
c_sequence pattern_arg = Derive.generator module_ "sequence" Tags.inst
    "Play a sequence of mridangam strokes. Each one takes the given `dur`, and\
    \ if the event is longer than the sequence, it is repeated to fill up\
    \ available space. If a whole cycle doesn't fit, clip from the end for a\
    \ positive event, or from the beginning for a negative one.\
    \ If `dur` is 0, then stretch to the sequence to fit the event."
    $ Sig.call ((,) <$> pattern_arg <*> dur_arg) $ \(pattern, dur) ->
    Sub.inverting $ \args -> do
        -- 1/4 because m_sequence scales by 4, since it expects
        -- Sequence.default_tempo.  TODO this is weird, fix it.
        let seq = map (1/4,) $ realize_sequence (Args.context args) pattern
        m_sequence seq dur (Args.range args) (Args.orientation args)

realize_sequence :: Derive.Context Score.Event -> Text
    -> [Maybe Derive.NoteDeriver]
realize_sequence ctx = map realize . Text.unpack
    where
    realize c
        | c == ' ' || c == '_' = Nothing
        | otherwise = Just $ do
            call <- Eval.get_generator (BaseTypes.Symbol (Text.singleton c))
            Eval.apply_generator ctx call []

pattern_arg :: Sig.Parser Text
pattern_arg = Sig.required_env "pattern" Sig.Unprefixed
    "Single letter stroke names.  `_` or space is a rest."

dur_arg :: Sig.Parser ScoreTime
dur_arg = Typecheck.non_negative <$> Sig.defaulted_env "dur" Sig.Both 0
    "Duration for each letter in the pattern. If 0, the pattern will\
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
        where note t (d, n) = (t + realToFrac d * step * scale, (t, n))
    -- Sequence.default_tempo has 4 nadai, so each one winds up being 1/4.
    scale = 4
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

-- * c_pattern

c_pattern :: Derive.Generator Derive.Note
c_pattern = Derive.generator module_ "pattern" Tags.inst
    "Like `seq`, but pick a standard pattern."
    $ Sig.call ((,,)
    <$> (fmap Typecheck.positive
        <$> Sig.required "n" "Number of strokes. If not given, and dur > 0,\
            \ then infer the number of strokes as the event_duration / dur.")
    <*> variation_arg <*> dur_arg
    ) $ \(maybe_strokes, variation, dur) -> Sub.inverting $ \args -> do
        strokes <- maybe (infer_strokes dur (Args.duration args)) return
            maybe_strokes
        notes <- Derive.require_right id $ infer_pattern strokes variation
        notes <- return $
            map (second (realize_stroke (Args.context args))) notes
        m_sequence notes dur (Args.range args) (Args.orientation args)

realize_stroke :: Derive.Context Score.Event -> Realize.Stroke Mridangam.Stroke
    -> Maybe Derive.NoteDeriver
realize_stroke ctx = fmap realize . stroke_call
    where
    realize sym = do
        call <- Eval.get_generator sym
        Eval.apply_generator ctx call []

stroke_call :: Realize.Stroke Mridangam.Stroke -> Maybe BaseTypes.CallId
stroke_call stroke = case stroke of
    Realize.Stroke stroke ->
        Just $ BaseTypes.Symbol $ Mridangam.stroke_to_call stroke
    Realize.Rest -> Nothing
    Realize.Pattern matras -> Just $ BaseTypes.Symbol $ "p" <> showt matras

infer_strokes :: ScoreTime -> ScoreTime -> Derive.Deriver Int
infer_strokes dur event_dur
    | dur > 0 = return $ floor (event_dur / dur)
    | otherwise = Derive.throw "can't infer both number of strokes and\
        \ duration of strokes simultaneously"

-- * c_infer_pattern

c_infer_pattern :: Derive.ValCall
c_infer_pattern = Derive.val_call module_ "infer-pattern" mempty
    "Pick a pattern based on the event duration."
    $ Sig.call ((,) <$> variation_arg <*> dur_arg) $
    \(variation, dur) args -> do
        let notes = round $ Args.duration args / dur
        Derive.require_right id $
            to_pattern . map snd =<< infer_pattern notes variation

variation_arg :: Sig.Parser Text
variation_arg = Sig.defaulted_env "var" Sig.Both default_variation
    ("Variation name. Possibilities are: "
        <> Doc.commas (map Doc.literal (Map.keys variations)))

to_pattern :: [Realize.Stroke Mridangam.Stroke] -> Either Text Text
to_pattern = fmap mconcat . traverse convert
    where
    convert (Realize.Stroke stroke) = Right $ Mridangam.stroke_to_call stroke
    convert Realize.Rest = Right "_"
    convert (Realize.Pattern matras) = Left $
        "pattern with another p" <> showt matras <> " can't go in a string"

infer_pattern :: Sequence.Matra -> Text
    -> Either Text [(Sequence.Duration, Realize.Stroke Mridangam.Stroke)]
infer_pattern dur variation = do
    patterns <- justErr ("unknown variation " <> showt variation) $
        Map.lookup variation variations
    notes <- justErr
        ("variation " <> showt variation <> " doesn't have duration: "
            <> showt dur)
        (Realize.lookup_pattern dur patterns)
    return $ Realize.tempo_to_duration $ Sequence.flatten notes

default_variation :: Text
default_variation = "d"

variations :: Map Text Mridangam.Patterns
variations = Map.fromList $
    [ (default_variation, Mridangam.default_patterns)
    , ("kt_kn_o", Mridangam.kt_kn_o)
    ] ++
    [ ("f567-" <> showt n, p) | (n, p) <- zip [0..] Mridangam.families567]
