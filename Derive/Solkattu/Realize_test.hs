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
import qualified Derive.Solkattu.Realize as Realize
import Derive.Solkattu.Realize (StartEnd(..))
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f = e_words . realize smap . mconcat
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
    let f = e_words . realize smap . mconcat
        smap = expect_right $ Realize.stroke_map
            [ (tat <> dit, [k, t])
            , (din, [od])
            ]
            where M.Strokes {..} = M.notes
    equal (f [tat, dit]) (Right "k t")
    -- TODO these could also go in Notation_test
    equal (f [Notation.dropM 1 (tat <> dit)]) (Right "t")
    equal (f [Notation.takeM 1 (tat <> dit)]) (Right "k")

    -- Groups keep finding fragments.
    equal (f [Notation.dropM 1 (tat <> dit <> din)]) (Right "t D")
    equal (f [Notation.dropM 2 (tat <> dit <> din)]) (Right "D")
    equal (f [Notation.dropM 3 (tat <> dit <> din)]) (Right "")
    equal (f [Notation.dropM 3 (tat <> dit <> din <> din)]) (Right "D")
    equal (f [Notation.takeM 2 (din <> tat <> dit)]) (Right "D k")
    equal (f [Notation.takeM 1 (din <> tat <> dit)]) (Right "D")
    equal (f [Notation.takeM 0 (din <> tat <> dit)]) (Right "")
    equal (f [Notation.rdropM 1 (tat <> dit)]) (Right "k")
    equal (f [Notation.rtakeM 1 (tat <> dit)]) (Right "t")
    equal (f (replicate 2 (Notation.takeM 1 (tat <> dit)))) (Right "k k")
    left_like (f [Notation.dropM 1 (dit <> dit)]) "sequence not found"
    -- With rests.
    equal (f [Notation.dropM 1 (tat <> __ <> dit <> __)]) (Right "_ t _")
    -- With a Pattern.
    equal (f [Notation.dropM 1 (tat <> dit <> Dsl.p5)]) (Right "t p5")
    -- Ensure groups are still in the output, and dropped sollus replaced
    -- with strokes.
    let e_group = fmap $ map $ (fmap ungroup . Sequence._mark) *** pretty
        ungroup (Sequence.GroupMark count (Solkattu.Group dropped side)) =
            (count, dropped, side)
    equal (e_group $ realize_s smap (tat <> dit))
        (Right [(Nothing, "k"), (Nothing, "t")])
    equal (e_group $ realize_s smap (Notation.dropM 1 (tat <> dit)))
        (Right [(Just (1, [Realize.stroke $ M.Valantalai M.Ki], Solkattu.Front),
            "t")])


e_words :: Pretty b => Either a [b] -> Either a Text
e_words = fmap (Text.unwords . map pretty)

test_realize_emphasis = do
    let f = second (map (fmap pretty)) . realize smap . mconcat
        smap = expect_right $
            Realize.stroke_map [(ta <> di, [Dsl.hv k, Dsl.lt t])]
            where M.Strokes {..} = M.notes
    equal (f [ta, di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realize_tag = do
    let f = second show_strokes . realize_s smap
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
    let f pmap = Realize.realize (Realize.realize_pattern pmap)
                (Realize.realize_sollu stroke_map)
            . map (meta,)
        meta = Sequence.Meta Nothing Sequence.default_tempo
    equal (show_strokes <$> f (M.families567 !! 0) [pattern 5])
        (Right "k t k n o")
    equal (show_strokes <$> f (M.families567 !! 1) [pattern 5])
        (Right "k _ t _ k _ k t o _")
    left_like (f (M.families567 !! 0) [pattern 3]) "no pattern for p3"

    let p = expect_right $ f (M.families567 !! 1) [pattern 5]
    equal (e_format $ format 80 Tala.adi_tala p) "K _ t _ k _ k t o _"
    equal (e_format $ format 15 Tala.adi_tala p) "K t k kto"

test_patterns = do
    let f = second (const ()) . Realize.patterns . map (first Solkattu.PatternM)
    let M.Strokes {..} = M.notes
    left_like (f [(2, [k])]) "2 /= realization matras 1"
    equal (f [(2, sd [k])]) (Right ())
    equal (f [(2, su [k, t, k, t])]) (Right ())
    equal (f [(2, [k, t])]) (Right ())

show_strokes :: [(tempo, Realize.Note M.Stroke)] -> Text
show_strokes = Text.unwords . map (pretty . snd)

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
    let f tala = e_format . format 80 tala . map (meta,)
        meta = Sequence.Meta Nothing Sequence.default_tempo
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
            . realize_s stroke_map
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

-- * util

realize :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note (Solkattu.Group Sollu) (Note Sollu)]
    -> Either Text [Realize.Note stroke]
realize smap = fmap (map snd) . realize_s smap

realize_s :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note (Solkattu.Group Sollu) (Note Sollu)]
    -> Either Text [(Realize.Meta (Realize.Stroke stroke), Realize.Note stroke)]
realize_s smap =
    Realize.realize Realize.keep_pattern (Realize.realize_sollu smap)
    . Sequence.flatten_with Sequence.default_tempo

k_realize :: Bool -> Tala.Tala -> Korvai.Sequence
    -> Either Text ([Korvai.MetaNote M.Stroke], Text)
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
    -> [(Sequence.Meta a, Realize.Note stroke)] -> Text
format = Realize.format Nothing
