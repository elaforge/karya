-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.Realize_test where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Util.TextUtil as TextUtil
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl (ta, di, __)
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as M
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Score as Score
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Solkattu(..), Sollu(..))
import qualified Derive.Solkattu.Tala as Tala

import Global


test_realize = do
    let f = second show_strokes . Realize.realize smap
            . map (Sequence.default_tempo,)
        sollu s = Sollu s Solkattu.NotKarvai Nothing
        smap = Realize.StrokeMap $ Map.fromList
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

    let chapu = Just (M.Valantalai M.Chapu)
    -- An explicit stroke will replace just that stroke.
    equal (f [sollu Na, Sollu Din Solkattu.NotKarvai chapu])
        (Right "n u")
    -- Not found is ok if it has an explicit stroke.
    equal (f [Sollu Tat Solkattu.NotKarvai chapu]) (Right "u")

pattern :: Sequence.Matra -> Solkattu stroke
pattern = Solkattu.Pattern . Solkattu.PatternM

rpattern :: Sequence.Matra -> Realize.Stroke stroke
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
    equal (e_format $ Realize.format 80 Tala.adi_tala p) "K _ t _ k _ k t o _"
    -- TODO when format is ok with 1 width it should be "k t k kto "

test_patterns = do
    let f = second (const ()) . Realize.patterns . map (first Solkattu.PatternM)
    let M.Strokes {..} = M.notes
    left_like (f [(2, [k])]) "2 /= realization matras 1"
    equal (f [(2, Dsl.slower [k])]) (Right ())
    equal (f [(2, Dsl.faster [k, t, k, t])]) (Right ())
    equal (f [(2, [k, t])]) (Right ())

show_strokes :: [(tempo, Realize.Stroke M.Stroke)] -> Text
show_strokes = Text.unwords . map (pretty . snd)

test_stroke_map = do
    let f = fmap (\(Realize.StrokeMap smap) -> Map.toList smap)
            . Realize.stroke_map
        M.Strokes {..} = M.notes
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])])
        (Right [([Ta, Di],
            [Just $ M.Valantalai M.Ki, Just $ M.Valantalai M.Ta])])
    left_like (f (replicate 2 (ta <> di, [k, t]))) "duplicate StrokeMap keys"
    left_like (f [(ta <> di, [k])]) "have differing lengths"
    left_like (f [(Dsl.tang <> Dsl.ga, [u, __, __])]) "differing lengths"
    left_like (f [(ta <> [Sequence.Note $ pattern 5], [k])])
        "only have plain sollus"

test_format = do
    let f tala = e_format . Realize.format 80 tala
            . map (Sequence.default_tempo,)
        n4 = [k, t, Realize.Rest, n]
        M.Strokes {..} = Realize.Stroke <$> M.strokes
        rupaka = Tala.rupaka_fast
    -- Emphasize every 4.
    equal (f rupaka n4) "K t _ n"
    equal (f rupaka (n4 <> n4)) "K t _ n K t _ n"
    -- Emphasis works in patterns.
    equal (f rupaka (n4 <> [rpattern 5] <> n4))
        "K t _ n P5------==k t _\nN"
    -- Patterns are wrapped properly.
    equal (f rupaka (n4 <> [rpattern 5] <> n4 <> [rpattern 5]))
        "K t _ n P5------==k t _\n\
        \N p5----==--"
    -- Emphasize according to the tala.
    let kook = [k, o, o, k]
    equal (f Tala.khanda_chapu (take (5*4) (cycle kook)))
        "K o o k k o o k K o o k K o o k k o o k"

tala4 :: Tala.Tala
tala4 = Tala.Tala [Tala.O, Tala.O] 0

test_format_ruler = do
    let run = fmap (first (capitalize_emphasis . Realize.format 80 tala4))
            . realize False tala4
    let tas nadai n = Dsl.nadai nadai (Dsl.repeat n ta)
    equal (run (tas 2 8)) $ Right
        ( "0   1   2   3   4\n\
          \K k k k K k k k"
        , ""
        )
    equal (run (tas 2 16)) $ Right
        ( "0   1   2   3   4\n\
          \K k k k K k k k\n\
          \K k k k K k k k"
        , ""
        )
    equal (run (tas 3 12)) $ Right
        ( "0     1     2     3     4\n\
          \K k k k k k K k k k k k"
        , ""
        )
    equal (run (tas 2 12 <> tas 3 6)) $ Right
        ( "0   1   2   3   4\n\
          \K k k k K k k k\n\
          \0   1   2     3     4\n\
          \K k k k K k k k k k"
        , ""
        )

test_format_lines = do
    let f width tala = fmap (extract . Realize.format_lines 2 width tala . fst)
            . realize True tala
        extract = map (map (Text.strip . mconcat . map snd))
    let tas n = Dsl.repeat n ta
    equal (f 16 tala4 (tas 8)) $ Right [["k k k k k k k k"]]

    -- Even aksharas break in the middle.
    equal (f 14 tala4 (tas 8)) $ Right [["k k k k", "k k k k"]]
    -- Uneven ones break before the width.
    equal (f 24 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k k k k k k k k k"]]
    equal (f 20 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k k k k k", "k k k k"]]
    equal (f 10 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k", "k k k k", "k k k k"]]
    equal (f 1 Tala.rupaka_fast (tas (4 * 3))) $
        Right [["k k k k", "k k k k", "k k k k"]]

test_format_break_lines = do
    let run width =
            fmap (capitalize_emphasis . Realize.format width tala4 . fst)
            . realize False tala4
    let tas n = Dsl.repeat n ta
    equal (run 80 (tas 16)) $ Right
        "0       1       2       3       4\n\
        \K k k k k k k k K k k k k k k k"
    equal (run 10 (tas 16)) $ Right
        "0   1   2\n\
        \Kkkkkkkk\n\
        \Kkkkkkkk"

test_format_nadai_change = do
    let f tala realize_patterns =
            fmap (first (capitalize_emphasis . Realize.format 50 tala))
            . realize realize_patterns tala
    let sequence = Dsl.faster (Dsl.__ <> Dsl.repeat 5 Dsl.p7)
            <> Dsl.nadai 6 (Dsl.tri Dsl.p7)
    let (out, warn) = expect_right $ f Tala.adi_tala True sequence
    equal (Text.lines out)
        [ "0       1       2       3       4"
        , "_k_t_knok t knok_t_knok t knok_t"
        -- TODO should be a ruler here
        , "_knok _ t _ k n o k _ T _ k n o k _ t _ k n o"
        ]
    equal warn ""
    -- 0123456701234567012345670123456701234560123450123450123450
    -- 0       1       2       3       4   |  5     6     7     8
    -- _k_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_kno

test_format_speed = do
    let f width = fmap (e_format . Realize.format width Tala.rupaka_fast)
            . Realize.realize stroke_map . Sequence.flatten
        thoms n = mconcat (replicate n Dsl.thom)
    equal (f 80 []) (Right "")
    equal (f 80 (thoms 8)) (Right "O o o o O o o o")
    equal (f 80 [nadai 3 $ thoms 6]) (Right "O o o O o o")
    equal (f 80 $ slower (thoms 4)) (Right "O _ o _ O _ o _")
    equal (f 80 $ thoms 2 <> faster (thoms 4) <> thoms 1)
        (Right "O _ o _ o o o o O _")
    equal (f 80 $ thoms 2 <> faster (faster (thoms 8)) <> thoms 1)
        (Right "O _ _ _ o _ _ _ o o o o o o o o O _ _ _")
    equal (f 80 $ slower (thoms 2) <> thoms 4) (Right "O _ o _ O o o o")
    equal (f 80 (Dsl.p5 <> Dsl.p5)) (Right "P5------==p5----==--")
    -- Use narrow spacing when there's isn't space, and p5 overlaps the next
    -- '-'.
    equal (f 10 (Dsl.p5 <> Dsl.p5)) (Right "P5--=p5-=-")

-- * util

realize :: Bool -> Tala.Tala -> Korvai.Sequence
    -> Either Text ([(Sequence.Tempo, Realize.Stroke M.Stroke)], Text)
realize realize_patterns tala = Korvai.realize Korvai.mridangam realize_patterns
    . Korvai.korvai tala mridangam

stroke_map :: Realize.StrokeMap M.Stroke
stroke_map = Realize.inst_stroke_map $ expect_right $ M.instrument [] mempty

mridangam :: Korvai.Instruments
mridangam = mempty
    { Korvai.inst_mridangam = Dsl.check $
        M.instrument (Score.standard_strokes ++ [(ta, [M.k M.notes])])
            M.default_patterns
    }

slower = (:[]) . Sequence.slower
faster = (:[]) . Sequence.faster

nadai :: Sequence.Nadai -> [Sequence.Note a] -> Sequence.Note a
nadai n = Sequence.TempoChange (Sequence.Nadai n)

e_format :: Text -> Text
e_format = capitalize_emphasis . drop_rulers

drop_rulers :: Text -> Text
drop_rulers = Text.strip . Text.unlines . filter (not . is_ruler) . Text.lines
    where is_ruler = Text.all Char.isDigit . Text.take 1

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
