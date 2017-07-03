-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Realize_test where
import Prelude hiding ((^))
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Util.TextUtil as TextUtil
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl ((^), __)
import Derive.Solkattu.DslSollu
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as M
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f = second show_strokes . Realize.realize smap
            . map (Sequence.default_tempo,)
        smap = Realize.simple_stroke_map
            [ ([Ta, Din], map Just [k, od])
            , ([Na, Din], map Just [n, od])
            , ([Ta], map Just [t])
            , ([Din, Ga], [Just od, Nothing])
            ]
        k = M.Valantalai M.Ki
        t = M.Valantalai M.Ta
        od = M.Both M.Thom M.Din
        n = M.Valantalai M.Nam
    equal (f [Rest, sollu Ta, Rest, Rest, sollu Din]) (Right "_ k _ _ D")
    equal (f [pattern 5, Rest, sollu Ta, sollu Din]) (Right "p5 _ k D")
    equal (f [sollu Ta, sollu Ta]) (Right "t t")
    equal (f [sollu Din, sollu Ga]) (Right "D _")
    equal (f [sollu Din, Rest, sollu Ga]) (Right "D _ _")
    left_like (f [sollu Din, sollu Din]) "sequence not found"

    let chapu = Just (Realize.stroke $ M.Valantalai M.Chapu)
    let set_chapu = Solkattu.modify_stroke (const chapu)
    -- An explicit stroke will replace just that stroke.
    equal (f [sollu Na, set_chapu (sollu Din)]) (Right "n u")
    -- Not found is ok if it has an explicit stroke.
    equal (f [set_chapu (sollu Tat)]) (Right "u")

test_realize_emphasis = do
    let f = second (map (fmap pretty . snd)) . Realize.realize smap
            . map (Sequence.default_tempo,)
        smap = expect_right $
            Realize.stroke_map [(ta <> di, [Dsl.hv k, Dsl.lt t])]
            where M.Strokes {..} = M.notes
    equal (f [sollu Ta, sollu Di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realize_tag = do
    let smap = expect_right $ Realize.stroke_map
            [ (ta <> ta, [p, p])
            , (ta, [k])
            , (1^ta, [t])
            ]
        M.Strokes {..} = M.notes
    let f = second show_strokes . Realize.realize smap
            . Sequence.flatten
    equal (f ta) (Right "k")
    equal (f (ta <> ta)) (Right "p p")
    -- Having a tag is more important than a longer match.
    equal (f (1^ta <> ta)) (Right "t k")
    equal (f (2^ta)) (Right "k")

sollu :: Sollu -> Note stroke
sollu s = Solkattu.Note (Solkattu.note s Nothing)

pattern :: Sequence.Matra -> Solkattu.Note stroke
pattern = Solkattu.Pattern . Solkattu.PatternM

rpattern :: Sequence.Matra -> Realize.Note stroke
rpattern = Realize.Pattern . Solkattu.PatternM

test_realize_patterns = do
    let f pmap = second (Text.unwords . map (pretty . snd)) . realize pmap
        realize pmap = Realize.realize mempty
            <=< Realize.realize_patterns pmap
            . map (Sequence.default_tempo,)
    equal (f (M.families567 !! 0) [pattern 5]) (Right "k t k n o")
    equal (f (M.families567 !! 1) [pattern 5]) (Right "k _ t _ k _ k t o _")
    left_like (f (M.families567 !! 0) [pattern 3]) "no pattern for p3"

    let p = expect_right $ realize (M.families567 !! 1) [pattern 5]
    equal (e_format $ format 80 Tala.adi_tala p) "K _ t _ k _ k t o _"
    -- TODO when format is ok with 1 width it should be "k t k kto "

test_patterns = do
    let f = second (const ()) . Realize.patterns . map (first Solkattu.PatternM)
    let M.Strokes {..} = M.notes
    left_like (f [(2, [k])]) "2 /= realization matras 1"
    equal (f [(2, Dsl.sd [k])]) (Right ())
    equal (f [(2, Dsl.su [k, t, k, t])]) (Right ())
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
    let f tala = e_format . format 80 tala . map (Sequence.default_tempo,)
        n4 = [k, t, Realize.Rest, n]
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
            . realize False tala4
    let tas nadai n = Dsl.nadai nadai (Dsl.repeat n ta)
    equal (run (tas 2 8)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k"
        , ""
        )
    equal (run (tas 2 16)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k\n\
          \K k k k K k k k"
        , ""
        )
    equal (run (tas 3 12)) $ Right
        ( "X     O     X     O     |\n\
          \K k k k k k K k k k k k"
        , ""
        )
    equal (run (tas 2 12 <> tas 3 6)) $ Right
        ( "X   O   X   O   |\n\
          \K k k k K k k k\n\
          \X   O   X     O     |\n\
          \K k k k K k k k k k"
        , ""
        )

test_format_lines = do
    let f stroke_width width tala =
            fmap (extract . Realize.format_lines stroke_width width tala . fst)
            . realize False tala
        extract = map (map (Text.strip . mconcat . map snd))
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

test_format_break_lines = do
    let run width = fmap (capitalize_emphasis . format width tala4 . fst)
            . realize False tala4
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
            . realize realize_patterns tala
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
            . Realize.realize stroke_map . Sequence.flatten
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

realize :: Bool -> Tala.Tala -> Korvai.Sequence
    -> Either Text ([(Sequence.Tempo, Realize.Note M.Stroke)], Text)
realize realize_patterns tala =
    head . Korvai.realize Korvai.mridangam realize_patterns
    . Korvai.korvai tala mridangam
    . (:[])

stroke_map :: Realize.StrokeMap M.Stroke
stroke_map = Realize.inst_stroke_map $ expect_right $ M.instrument [] mempty

mridangam :: Korvai.Instruments
mridangam = mempty
    { Korvai.inst_mridangam = Dsl.check $
        M.instrument [(ta, [M.k M.notes])] M.default_patterns
    }

sd = (:[]) . Sequence.change_speed (-1)
su = (:[]) . Sequence.change_speed 1

nadai :: Sequence.Nadai -> [Sequence.Note a] -> Sequence.Note a
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

format :: Pretty stroke => Int -> Tala.Tala
    -> [(Sequence.Tempo, Realize.Note stroke)] -> Text
format = Realize.format Nothing
