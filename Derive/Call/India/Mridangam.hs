-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Library of standard mridangam patterns.
module Derive.Call.India.Mridangam where
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.BaseTypes as BaseTypes

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("p1", c_pattern_once pattern_arg)
    , ("pn", c_pattern_times)
    , ("pr", c_pattern_repeat False)
    , ("Pr", c_pattern_repeat True)

    -- standard patterns
    , ("tari", c_pattern_once (pure faran_base))
    , ("tk", c_pattern_once (pure "k+"))
    , ("tknk", c_pattern_once (pure "k+n+"))
    ]

val_calls :: [Derive.LookupCall Derive.ValCall]
val_calls = Derive.call_map
    [ ("pi", c_infer_pattern)
    ]

module_ :: Module.Module
module_ = "india" <> "mridangam"

c_pattern_once :: Sig.Parser Text -> Derive.Generator Derive.Note
c_pattern_once pattern_arg = Derive.generator module_ "pattern" Tags.inst
    "Emit a pattern, fitted into the note duration."
    $ Sig.call pattern_arg $ \pattern -> Sub.inverting $ \args -> do
        let notes = stretch_to_range (Args.range args) $
                realize_pattern (Args.context args) pattern
        mconcat [Derive.place start 0 note | (start, Just note) <- notes]

stretch_to_range :: (ScoreTime, ScoreTime) -> [a] -> [(ScoreTime, a)]
stretch_to_range (start, end) xs = zip (Seq.range_ start dur) xs
    where dur = (end - start) / fromIntegral (length xs)

c_pattern_times :: Derive.Generator Derive.Note
c_pattern_times = Derive.generator module_ "pattern" Tags.inst
    "Repeat a pattern a certain number of times, fitted into the note duration."
    $ Sig.call ((,)
    <$> pattern_arg
    <*> Sig.defaulted "times" 3 "Repeat the pattern this many times."
    ) $ \(pattern, times) -> Sub.inverting $ \args -> do
        let notes = stretch_to_range (Args.range args) $
                concat $ List.replicate times $
                realize_pattern (Args.context args) pattern
        mconcat [Derive.place start 0 note | (start, Just note) <- notes]

c_pattern_repeat :: Bool -> Derive.Generator Derive.Note
c_pattern_repeat clip_start = Derive.generator module_ "pattern" Tags.inst
    "Repeat a pattern, where each note has the given duration. The first\
    \ variant clips before the end of the note, and the second variant\
    \ lines the end of the pattern up to the end of the note."
    $ Sig.call ((,)
    <$> pattern_arg
    <*> Sig.defaulted_env_quoted "dur" Sig.Prefixed
        (BaseTypes.quoted "ts" [BaseTypes.str "e"])
        "Duration for each letter in the pattern."
    ) $ \(pattern, dur) -> Sub.inverting $ \args -> do
        let notes = pattern_repeat clip_start (Args.range args) dur $
                realize_pattern (Args.context args) pattern
        mconcat [Derive.place start 0 note | (start, note) <- notes]

pattern_repeat :: Bool -> (ScoreTime, ScoreTime) -> ScoreTime
    -> [Maybe Derive.NoteDeriver] -> [(ScoreTime, Derive.NoteDeriver)]
pattern_repeat clip_start (start, end) dur pattern
    | clip_start = strip $ reverse $ takeWhile ((>=start) . fst) $
        zip (Seq.range_ end (-dur)) (cycle (reverse pattern))
    | otherwise = strip $ takeWhile ((<end) . fst) $
        zip (Seq.range_ start dur) (cycle pattern)
    where strip xs = [(a, b) | (a, Just b) <- xs]

pattern_arg :: Sig.Parser Text
pattern_arg = Sig.required_env "pattern" Sig.Unprefixed
    "Single letter stroke names.  `_` or space is a rest."

realize_pattern :: Derive.Context Score.Event -> Text
    -> [Maybe Derive.NoteDeriver]
realize_pattern ctx = map realize . Text.unpack
    where
    realize c
        | c == ' ' || c == '_' = Nothing
        | otherwise = Just $ do
            call <- Eval.get_generator (BaseTypes.Symbol (Text.singleton c))
            Eval.apply_generator ctx call []

-- * infer-pattern

c_infer_pattern :: Derive.ValCall
c_infer_pattern = Derive.val_call module_ "infer-pattern" mempty
    "Pick a pattern based on the event duration."
    $ Sig.call ((,)
    <$> Sig.defaulted "var" 0 "Variation."
    <*> Sig.defaulted "dur" 0.25 "Duration for each note."
    ) $ \(var, dur) args -> do
        let notes = round $ Args.duration args / dur
        Derive.require
            ("invalid variation: dur " <> showt (Args.duration args)
                    <> " for var " <> showt var) $
            infer_pattern notes var

infer_pattern :: Int -> Int -> Maybe Text
infer_pattern dur var = IntMap.lookup dur =<< Seq.at patterns var

patterns :: [IntMap.IntMap Text]
patterns =
    [ IntMap.fromList [(5, p5), (6, p6), (7, p7)]
    | (p5, p6, p7) <- zip3 pattern5 pattern6 pattern7
    ]

pattern5 :: [Text]
pattern5 =
    [ "ktkno"
    , "k t k kto "
    , "k t k kno "
    , "kt+k+ktkno"
    , "ktkt+k+to "
    , "n kt+k+to "
    , "u kt+k+to "
    , "k t kt kno"
    , "k+kD ktkno"
    ]

pattern6 :: [Text]
pattern6 =
    [ "kt kno"
    , "k t   k kto "
    , "k t   k kno "
    , "k+kt+k+ktkno"
    , "+ ktkt+k+to "
    , "+ n kt+k+to "
    , "+ u kt+k+to "
    , "k+k t kt kno"
    , "k+ kD kt kno"
    ]

pattern7 :: [Text]
pattern7 =
    [ "k t kno"
    , "k   t   k kto "
    , "k   t   k kno "
    , "k+n+kt+k+ktkno"
    , "k + ktkt+k+to "
    , "k + n kt+k+to "
    , "k + u kt+k+to "
    , "k+n+k t kt kno"
    , "k+  kD k t kno"
    ]

-- These are not exposed in any way, and I'm not even sure how they should be
-- exposed.

faran_base :: Text
faran_base = "n+u+kt+k" -- naka tiku tari kita

farans :: [[Text]]
farans = concat
    [ map (make "+n+k" "+n+k")
        [ "ktkn+ktk"
        , "ookn+ktk"
        , "oonn+ktk"
        , "otkn+ktk"
        , "D Dn+ktk"
        , "odon+ktk"
        , "Tkon+ktk"
        , "+u n+ktk"
        , "ou n+ktk"
        ]
    , map (make "ou k" "ou k")
        [ "ou kkook"
        , "ou+kkook"
        , "okou ktk"
        , "okou+ktk"
        ]
    , map (make "o k " "ok+ktk")
        [ "okookook"
        , "o kokoTk"
        , "ookokoTk"
        , "o ktkoTk"
        , "ooktkoTk"
        , "k ktkoTk"
        , "k+ktkoTk"
        ]
    , [ make "ookt" "ookt" "+kookt+k"
      , make "o k " "ok+kt" "nk++kook"
      , make "noK " "K u " "noK u +k"
      ]
    ]
    where
    make fill1 fill2 start =
        [ pattern <> pattern
        , start <> start <> pattern
        , short <> short <> fill1 <> pattern
        , short <> short <> short <> fill2
            <> Text.drop (Text.length pattern - rest) pattern
        ]
        where
        rest = 32 - Text.length short * 3 - Text.length fill2
        pattern = start <> faran_base
        short = Text.take 6 start
