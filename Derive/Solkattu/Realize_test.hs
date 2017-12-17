-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Realize_test where
import Prelude hiding ((^))
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import Util.Test
import qualified Util.TextUtil as TextUtil

import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl ((^), __)
import Derive.Solkattu.DslSollu
import qualified Derive.Solkattu.Instrument.Mridangam as M
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Notation as Notation
import Derive.Solkattu.Notation (takeM, dropM, rdropM)
import qualified Derive.Solkattu.Realize as Realize
import Derive.Solkattu.Realize (StartEnd(..))
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f = e_words . realize_n smap . mconcat
        smap = expect_right $ Realize.stroke_map
            [ (ta <> din, [k, od])
            , (na <> din, [n, od])
            , (ta, [t])
            , (din <> __ <> ga, [od, __])
            ]
            where M.Strokes {..} = M.notes
    equal (f [__, ta, __, __, din]) (Right "_ k _ _ D")
    equal (f [Notation.sarvaM 4]) (Right "= = = =")
    equal (f [Dsl.p5, __, ta, din]) (Right "p5 _ k D")
    equal (f [ta, ta]) (Right "t t")
    equal (f [din, ga]) (Right "D _")
    equal (f [din, __, ga]) (Right "D _ _")
    left_like (f [din, din]) "sequence not found"

test_realize_groups = do
    let f = e_words . realize_n smap
        smap = expect_right $ Realize.stroke_map
            [ (taka, [k, t])
            , (din, [od])
            ]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
    equal (f taka) (Right "k t")

    -- TODO these could also go in Notation_test
    equal (f $ dropM 1 taka) (Right "t")
    equal (f $ takeM 1 taka) (Right "k")
    equal (f $ takeM 2 (taka <> din)) (Right "k t")
    equal (f $ dropM 2 (taka <> din)) (Right "D")
    equal (f $ dropM 1 $ su taka) $ Right ""
    equal (f $ su $ dropM 1 taka) $ Right "t"

    -- Groups keep finding fragments.
    equal (f $ dropM 1 $ taka <> din) (Right "t D")
    equal (f $ dropM 2 $ taka <> din) (Right "D")
    equal (f $ dropM 3 $ taka <> din) (Right "")
    equal (f $ dropM 3 $ taka <> din <> din) (Right "D")
    equal (f $ takeM 2 $ din <> taka) (Right "D k")
    equal (f $ takeM 1 $ din <> taka) (Right "D")
    equal (f $ takeM 0 $ din <> taka) (Right "")
    equal (f $ rdropM 1 taka) (Right "k")
    equal (f $ Notation.rtakeM 1 taka) (Right "t")
    equal (f $ mconcat $ replicate 2 $ takeM 1 taka) (Right "k k")
    left_like (f $ dropM 1 (ka <> ka)) "sequence not found"
    -- With rests.
    equal (f $ dropM 1 $ ta <> __ <> ka <> __) (Right "_ t _")
    -- With a Pattern.
    equal (f $ dropM 1 $ taka <> Dsl.p5) (Right "t p5")

test_realize_groups_output = do
    -- Ensure groups are still in the output, and dropped sollus replaced
    -- with strokes.
    let f = extract . realize smap
        smap = expect_right $ Realize.stroke_map [(taka, [k, t])]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
        extract = fmap $ Text.unwords . map fmt
        fmt (Sequence.FGroup _ _ g) = pretty g
        fmt (Sequence.FNote _ n) = pretty n
    equal (f taka) (Right "k t")
    equal (f $ dropM 1 taka) (Right "([k], Before) t")
    equal (f $ rdropM 1 taka) (Right "([t], After) k")

test_realize_groups_nested = do
    let f = fmap (mconcatMap pretty) . realize_n smap
        smap = expect_right $ Realize.stroke_map [(nakita, [n, k, t])]
            where M.Strokes {..} = M.notes
        nakita = na <> ki <> ta
    equal (f $ Notation.reduceTo 1 1 nakita) $ Right $ mconcat
        [ "nkt"
        , "kt"
        , "t"
        ]

    -- nested reduction
    equal (f $ Notation.reduceTo 2 2 $ Notation.reduceTo 1 1 nakita) $
        Right $ mconcat
        [ "nkt"
        , "kt"
        , "t"

        , "t"
        , "kt"
        , "t"

        , "t"
        , "t"
        ]

    -- 0   0   nkt
    --     1/4 nkt
    --     1/2 nkt
    --
    -- 1/2 0   nkt
    --     1/4 nkt
    --     1/2 nkt
    --
    -- 1   0   nkt
    --     1/4 nkt
    --     1/2 nkt
    --
    -- 1.5 0   nkt
    --     1/4 nkt
    --     1/2 nkt

    -- dropM 4 doesn't count the second na, because it's already dropped.
    equal (f $ dropM 4 $ nakita <> dropM 1 nakita) $
        Right "t"

    -- This is the sandi situtaion.
    -- The dropM 2 drops the first group with the leading Na, so the
    -- fact that there was one is lost:
    --  group [na] [ki, ta] . [na, ki, ta] ==>
    --      group [ki, ta, na] [ki, ta]
    --
    -- Instead it could collect the [na] from the group at the front:
    --      ==> group [na, ki, ta, na] [ki, ta]
    -- What else?  It would have to omit the [ki, ta] from the dropped group
    -- from the prefix.  I guess the rule would be don't include parts of other
    -- groups in a group's prefix.
    equal (f $ dropM 2 $ dropM 1 nakita <> nakita) $ Right "nkt"
    equal (f $ dropM 1 $ nakita <> dropM 1 nakita) $ Right "ktkt"

e_words :: Pretty b => Either a [b] -> Either a Text
e_words = fmap (Text.unwords . map pretty)

test_realize_emphasis = do
    let f = second (map (fmap pretty)) . realize_n smap . mconcat
        smap = expect_right $
            Realize.stroke_map [(ta <> di, [Dsl.hv k, Dsl.lt t])]
            where M.Strokes {..} = M.notes
    equal (f [ta, di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realize_tag = do
    let f = e_words . realize_n smap
        smap = expect_right $ Realize.stroke_map
            [ (ta <> ta, [p, p])
            , (ta, [k])
            , (1^ta, [t])
            ] where M.Strokes {..} = M.notes
    equal (f ta) (Right "k")
    equal (f (ta <> ta)) (Right "p p")
    -- Having a tag is more important than a longer match.
    equal (f (1^ta <> ta)) (Right "t k")
    equal (f (2^ta)) (Right "k")

sollu :: Sollu -> Note Sollu
sollu s = Solkattu.Note (Solkattu.note s)

pattern :: Sequence.Matra -> Solkattu.Note stroke
pattern = Solkattu.Pattern . Solkattu.PatternM

rpattern :: Sequence.Matra -> Realize.Note stroke
rpattern = Realize.Pattern . Solkattu.PatternM

test_realize_patterns = do
    let f pmap =
            Realize.realize (Realize.realize_pattern pmap)
                (Realize.realize_sollu stroke_map)
            . Sequence.flatten
    let e_strokes = e_words . fmap Sequence.flattened_notes
    equal (e_strokes $ f (M.families567 !! 0) Dsl.p5)
        (Right "k t k n o")
    equal (e_strokes $ f (M.families567 !! 1) Dsl.p5)
        (Right "k _ t _ k _ k t o _")
    -- This ensures that 'Realize.realize' fixes the FGroup count if
    -- realize_patterns changes it.
    equal (e_strokes $ f M.default_patterns $ rdropM 0 $ sd Dsl.p5)
        (Right "k t k n o")
    left_like (f (M.families567 !! 0) (Dsl.pat 3)) "no pattern for p3"

    let p = expect_right $ f (M.families567 !! 1) Dsl.p5
    equal (e_format $ format 80 Tala.adi_tala p) "K _ t _ k _ k t o _"
    equal (e_format $ format 15 Tala.adi_tala p) "K t k kto"

test_patterns = do
    let f = second (const ()) . Realize.patterns . map (first Solkattu.PatternM)
    let M.Strokes {..} = M.notes
    left_like (f [(2, [k])]) "2 /= realization matras 1"
    equal (f [(2, sd [k])]) (Right ())
    equal (f [(2, su [k, t, k, t])]) (Right ())
    equal (f [(2, [k, t])]) (Right ())

test_stroke_map = do
    let f = fmap (\(Realize.StrokeMap smap) -> Map.toList smap)
            . Realize.stroke_map
        M.Strokes {..} = M.notes
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])]) $ Right
        [ ( (Nothing, [Ta, Di])
          , map (Just . Realize.stroke) [M.Valantalai M.Ki, M.Valantalai M.Ta]
          )
        ]
    -- Last one wins.
    equal (f [(ta, [k]), (ta, [t])]) $ Right
        [((Nothing, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ta))])]
    equal (f [(1 ^ ta, [k])]) $ Right
        [((Just 1, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ki))])]
    left_like (f [(ta <> di, [k])]) "have differing lengths"
    left_like (f [(tang <> ga, [u, __, __])]) "differing lengths"
    left_like (f [(ta <> [Sequence.Note $ pattern 5], [k])])
        "only have plain sollus"

test_format = do
    let f tala = e_format . format 80 tala
            . map (Sequence.FNote Sequence.default_tempo)
        n4 = [k, t, Realize.Space Solkattu.Rest, n]
        M.Strokes {..} = Realize.Note . Realize.stroke <$> M.strokes
        rupaka = Tala.rupaka_fast
    -- Emphasize every 4.
    equal (f rupaka n4) "K t _ n"
    equal (f rupaka (n4 <> n4)) "K t _ n K t _ n"
    -- Emphasis works in patterns.
    equal (f rupaka (n4 <> [rpattern 5] <> n4))
        "K t _ n P5------==k t _ N"
    -- Patterns are wrapped properly.
    equal (f rupaka (n4 <> [rpattern 5] <> n4 <> [rpattern 5]))
        "K t _ n P5------==k t _\n\
        \N p5----==--"
    -- Emphasize according to the tala.
    let kook = [k, o, o, k]
    equal (f Tala.khanda_chapu (take (5*4) (cycle kook)))
        "K o o k k o o k K o o k K o o k k o o k"

tala4 :: Tala.Tala
tala4 = Tala.Tala "tala4" [Tala.O, Tala.O] 0

test_format_ruler = do
    let run = fmap (first (capitalize_emphasis . format 80 tala4))
            . k_realize False tala4
    let tas nadai n = Dsl.nadai nadai (Dsl.repeat n ta)
    equal_t (run (tas 2 8)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k"
        , ""
        )
    equal_t (run (tas 2 16)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k\n\
          \K k k k K k k k"
        , ""
        )
    equal_t (run (tas 3 12)) $ Right
        ( "X     O     X     O     |\n\
          \K k k k k k K k k k k k"
        , ""
        )

    equal_t (run (tas 2 12 <> tas 3 6)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k\n\
          \X   O   X     O     |\n\
          \K k k k K k k k k k"
        , ""
        )
    -- A final stroke won't cause the ruler to reappear.
    equal_t (run (tas 2 16 <> ta)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k\n\
          \K k k k K k k k K"
        , ""
        )

equal_t :: (CallStack.Stack, Eq a, Show a) => Either Text (Text, a)
    -> Either Text (Text, a) -> IO Bool
equal_t = equal_fmt (either id fst)

test_format_lines = do
    let f stroke_width width tala =
            fmap (extract . Realize.format_lines stroke_width width tala . fst)
            . k_realize False tala
        extract = map $ map $ Text.strip . mconcat . map (Realize._text . snd)
    let tas n = Dsl.repeat n ta

    equal (f 2 16 tala4 (tas 8)) $ Right [["k k k k k k k k"]]
    -- Even aksharas break in the middle.
    equal (f 2 14 tala4 (tas 8)) $ Right [["k k k k", "k k k k"]]

    -- Break multiple avartanams and lines.
    let ta2 = "k _ _ _ k _ _ _"
    equal (f 2 8 tala4 (sd (sd (tas 8)))) $ Right
        [ [ta2, ta2]
        , [ta2, ta2]
        ]
    -- If there's a final stroke on sam, append it to the previous line.
    equal (f 2 8 tala4 (sd (sd (tas 9)))) $ Right
        [ [ta2, ta2]
        , [ta2, ta2 <> " k"]
        ]

    -- Uneven ones break before the width.
    equal (f 2 24 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k k k k k k k k k"]]
    equal (f 2 20 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k k k k k", "k k k k"]]
    equal (f 2 10 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k", "k k k k", "k k k k"]]
    equal (f 2 1 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k", "k k k k", "k k k k"]]

    equal (f 1 80 Tala.rupaka_fast (Dsl.pat 4)) $ Right [["p4--"]]
    equal (f 2 80 Tala.rupaka_fast (Dsl.pat 4)) $ Right [["p4------"]]

test_format_symbol = do
    let f = fmap (extract . Realize.format_lines 2 80 tala . fst)
            . k_realize False tala
        extract = map ((\(Realize.Symbol _ b c) -> (b, c)) . snd)
            . head . head
        tala = Tala.rupaka_fast
    let group = Notation.dropM 0
    let tas n = Dsl.repeat n ta
    equal (f (group $ tas 4 <> group (tas 4))) $ Right
        [ (True, [Start]), (False, []), (False, []), (False, [])
        , (True, [Start]), (False, []), (False, []), (False, [End, End])
        ]

test_annotate_groups = do
    let f = map (second (pretty . snd))
            . Realize.annotate_groups
            . Sequence.normalize_speed Tala.adi_tala
            . Sequence.flatten
    equal (f (ta <> ki)) [([], "ta"), ([], "ki")]
    equal (f (Notation.group ta <> ki)) [([Start, End], "ta"), ([], "ki")]
    equal (f (Notation.group (ta <> ki))) [([Start], "ta"), ([End], "ki")]
    equal (f (Notation.group (ta <> Notation.group ki)))
        [([Start], "ta"), ([Start, End, End], "ki")]
    equal (f (Notation.group (Notation.group (ta <> ki))))
        [([Start, Start], "ta"), ([End, End], "ki")]

test_format_break_lines = do
    let run width = fmap (capitalize_emphasis . format width tala4 . fst)
            . k_realize False tala4
    let tas n = Dsl.repeat n ta
    equal (run 80 (tas 16)) $ Right
        "X       O       X       O       |\n\
        \K k k k k k k k K k k k k k k k"
    equal (run 10 (tas 16)) $ Right
        "X   O   X\n\
        \Kkkkkkkk\n\
        \Kkkkkkkk"

test_format_nadai_change = do
    let f tala realize_patterns =
            fmap (first (capitalize_emphasis . format 50 tala))
            . k_realize realize_patterns tala
    let sequence = Dsl.su (Dsl.__ <> Dsl.repeat 5 Dsl.p7)
            <> Dsl.nadai 6 (Dsl.tri Dsl.p7)
    let (out, warn) = expect_right $ f Tala.adi_tala True sequence
    equal (Text.lines out)
        [ "0       1       2       3       X"
        , "_k_t_knok t knok_t_knok t knok_t"
        -- TODO should be a ruler here
        , "_knok _ t _ k n o k _ T _ k n o k _ t _ k n o"
        ]
    equal warn ""
    -- 0123456701234567012345670123456701234560123450123450123450
    -- 0       1       2       3       4   |  5     6     7     8
    -- _k_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_kno

test_format_speed = do
    let f width = fmap (e_format . format width Tala.rupaka_fast)
            . realize stroke_map
        thoms n = mconcat (replicate n thom)
    equal (f 80 []) (Right "")
    equal (f 80 (thoms 8)) (Right "O o o o O o o o")
    equal (f 80 [nadai 3 $ thoms 6]) (Right "O o o O o o")
    equal (f 80 $ sd (thoms 4)) (Right "O _ o _ O _ o _")
    equal (f 80 $ thoms 2 <> su (thoms 4) <> thoms 1)
        (Right "O _ o _ o o o o O _")
    equal (f 80 $ thoms 2 <> su (su (thoms 8)) <> thoms 1)
        (Right "O _ _ _ o _ _ _ o o o o o o o o O _ _ _")
    equal (f 80 $ sd (thoms 2) <> thoms 4) (Right "O _ o _ O o o o")
    equal (f 80 (Dsl.p5 <> Dsl.p5)) (Right "P5------==p5----==--")
    -- Use narrow spacing when there's isn't space, and p5 overlaps the next
    -- '-'.
    equal (f 10 (Dsl.p5 <> Dsl.p5)) (Right "P5--=p5-=-")

-- * verify_alignment

test_verify_alignment = do
    let f = verify_alignment tdkt_smap Tala.adi_tala
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f []) (Right Nothing)
    equal (f (take 4 tdkt)) $ Right $ Just
        (4, "korvai should end on or before sam: avartanam 1, akshara 1 + 0")
    equal (f (take 6 tdkt)) $ Right (Just (6,
        "korvai should end on or before sam: avartanam 1, akshara 1 + 1/2"))
    equal (f (take (8*4) tdkt)) (Right Nothing)
    equal (f (Dsl.speed (-2) $ take 8 tdkt)) (Right Nothing)
    -- Ok to end on sam, even with trailing rests.
    equal (f (Dsl.speed (-2) $ take 9 tdkt <> __ <> __)) (Right Nothing)
    -- But I don't drop rests because sometimes they make it line up.
    equal (f (Dsl.speed (-2) $ take 7 tdkt <> __)) (Right Nothing)

    equal (f (Dsl.speed (-2) $ take 4 tdkt <> Dsl.akshara 4 <> take 4 tdkt))
        (Right Nothing)
    equal (f (take 3 tdkt <> Dsl.akshara 4 <> take 5 tdkt)) $ Right
        (Just (3, "expected akshara 4, but at avartanam 1, akshara 0 + 3/4"))

test_verify_alignment_nadai_change = do
    let f = verify_alignment tdkt_smap Tala.adi_tala
        tdkt = ta <> di <> ki <> ta
    -- Change nadai in the middle of an akshara.
    equal (f (take 2 tdkt <> Dsl.nadai 6 (take 3 tdkt))) $
        Right (Just (5,
            "korvai should end on or before sam: avartanam 1, akshara 1 + 0"))

    -- More complicated example:
    -- 0 __ Ta __ di __ ki th tm
    -- 1 Ta __ di __ Ki th tm Ta
    -- 2 __ di __ ki Th tm Ta __
    -- 3 di __ ki th Tm Ta __ di
    -- 4 __ ki th tm Ta __ di
    --               nadai 6
    -- 5 -_ ki th Tm ta __
    -- 6 di __ ki th tm ta
    -- 7 __ di __ ki th tm
    let sequence p7 = Dsl.nadai 8 (__ <> Dsl.repeat 5 p7)
            <> Dsl.nadai 6 (Dsl.tri p7)
    equal (f (sequence (ta <> __ <> di <> __ <> ki <> tha <> thom)))
        (Right Nothing)
    equal (f (sequence Dsl.p7)) (Right Nothing)

tdkt_smap :: Realize.StrokeMap M.Stroke
tdkt_smap = expect_right $ Realize.stroke_map
    [ (ta, [k])
    , (di, [t])
    , (ki, [p])
    , (tha, [k])
    , (thom, [o])
    ]
    where M.Strokes {..} = M.notes

verify_alignment :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> Tala.Tala -> Korvai.Sequence -> Either Text (Maybe (Int, Text))
verify_alignment smap tala =
    fmap (Realize.verify_alignment tala . Sequence.tempo_notes) . realize smap

-- * util

realize_n :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Note stroke]
realize_n smap = fmap Sequence.flattened_notes . realize smap

realize :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Realized stroke]
realize smap =
    Realize.realize Realize.keep_pattern (Realize.realize_sollu smap)
    . Sequence.flatten

k_realize :: Bool -> Tala.Tala -> Korvai.Sequence
    -> Either Text ([Korvai.Flat M.Stroke], Text)
k_realize realize_patterns tala =
    head . Korvai.realize Korvai.mridangam realize_patterns
    . Korvai.korvai tala mridangam
    . (:[])

stroke_map :: Realize.StrokeMap M.Stroke
stroke_map = expect_right $ Realize.stroke_map
    [ (thom, [o])
    ]
    where M.Strokes {..} = M.notes

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.inst_mridangam = Dsl.check $
        Realize.instrument [(ta, [M.k M.notes])] M.default_patterns
    }

sd, su :: [Sequence.Note g a] -> [Sequence.Note g a]
sd = (:[]) . Sequence.change_speed (-1)
su = (:[]) . Sequence.change_speed 1

nadai :: Sequence.Nadai -> [Sequence.Note g a] -> Sequence.Note g a
nadai n = Sequence.TempoChange (Sequence.Nadai n)

e_format :: Text -> Text
e_format = capitalize_emphasis . drop_rulers

drop_rulers :: Text -> Text
drop_rulers = Text.strip . Text.unlines . filter (not . is_ruler) . Text.lines
    where
    is_ruler t = Text.all Char.isDigit (Text.take 1 t)
        || "X" `Text.isPrefixOf` t

-- | Replace emphasis with capitals, so spacing is preserved.
capitalize_emphasis :: Text -> Text
capitalize_emphasis =
    TextUtil.mapDelimited True '!' (Text.replace "-" "=" . Text.toUpper)
    . Text.replace "\ESC[0m" "!" . Text.replace "\ESC[1m" "!"

state_pos :: Sequence.State -> (Int, Tala.Akshara, Sequence.Duration)
state_pos state =
    ( Sequence.state_avartanam state
    , Sequence.state_akshara state
    , Sequence.state_matra state
    )

format :: Solkattu.Notation stroke => Int -> Tala.Tala
    -> [Sequence.Flat g (Realize.Note stroke)] -> Text
format = Realize.format Nothing
