-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Realize_test where
import Prelude hiding ((^))
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import Util.Test
import qualified Util.TextUtil as TextUtil

import qualified Solkattu.Dsl as Dsl
import Solkattu.Dsl ((^), __)
import Solkattu.DslSollu
import qualified Solkattu.Instrument.Mridangam as M
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Notation as Notation
import Solkattu.Notation (takeM, dropM, rdropM)
import qualified Solkattu.Realize as Realize
import Solkattu.Realize (StartEnd(..))
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Solkattu.Tala as Tala

import Global


test_realize = do
    let f = eWords . realizeN smap . mconcat
        smap = expect_right $ Realize.strokeMap
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

test_realizeGroups = do
    let f = eWords . realizeN smap
        smap = expect_right $ Realize.strokeMap
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

    left_like (f $ ka <> ka) "sequence not found"
    left_like (f $ dropM 1 (ka <> ka)) "sequence not found"
    -- I leave the extra 'k' in the output, otherwise 'sequence not found' will
    -- be suppressed by 'group split too long' error.
    left_like (f $ dropM 1 $ taka <> dropM 1 (ka<>ka)) "k  t*sequence not found"

    -- With rests.
    equal (f $ dropM 1 $ ta <> __ <> ka <> __) (Right "_ t _")
    -- With a Pattern.
    equal (f $ dropM 1 $ taka <> Dsl.p5) (Right "t p5")

test_realizeGroupsOutput = do
    -- Ensure groups are still in the output, and dropped sollus replaced
    -- with strokes.
    let f = extract . realize smap
        smap = expect_right $ Realize.strokeMap [(taka, [k, t])]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
        extract = fmap $ Text.unwords . map fmt
        fmt (Sequence.FGroup _ g children) =
            pretty g <> "(" <> Text.unwords (map fmt children) <> ")"
        fmt (Sequence.FNote _ n) = pretty n
    equal (f taka) (Right "k t")
    equal (f $ dropM 1 taka) (Right "([k], Before)(t)")
    equal (f $ rdropM 1 taka) (Right "([t], After)(k)")

test_realizeGroupsNested = do
    let f = fmap (mconcatMap pretty) . realizeN smap
        smap = expect_right $ Realize.strokeMap [(nakita, [n, k, t])]
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

eWords :: Pretty b => Either a [b] -> Either a Text
eWords = fmap (Text.unwords . map pretty)

test_realizeEmphasis = do
    let f = second (map (fmap pretty)) . realizeN smap . mconcat
        smap = expect_right $
            Realize.strokeMap [(ta <> di, [Dsl.hv k, Dsl.lt t])]
            where M.Strokes {..} = M.notes
    equal (f [ta, di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realizeTag = do
    let f = eWords . realizeN smap
        smap = expect_right $ Realize.strokeMap
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

test_realizePatterns = do
    let f pmap = Realize.formatError
            . Realize.realize (Realize.realizePattern pmap)
                (Realize.realizeSollu strokeMap)
            . Sequence.flatten
    let eStrokes = eWords . fmap Sequence.flattenedNotes
    equal (eStrokes $ f (M.families567 !! 0) Dsl.p5)
        (Right "k t k n o")
    equal (eStrokes $ f (M.families567 !! 1) Dsl.p5)
        (Right "k _ t _ k _ k t o _")
    -- This ensures that 'Realize.realize' fixes the FGroup count if
    -- realizePatterns changes it.
    equal (eStrokes $ f M.defaultPatterns $ rdropM 0 $ sd Dsl.p5)
        (Right "k t k n o")
    left_like (f (M.families567 !! 0) (Dsl.pat 3)) "no pattern for p3"

    let p = expect_right $ f (M.families567 !! 1) Dsl.p5
    equal (eFormat $ format 80 Tala.adi_tala p) "k _ t _ k _ k t o _"
    equal (eFormat $ format 15 Tala.adi_tala p) "k t k kto"

test_patterns = do
    let f = second (const ()) . Realize.patterns . map (first Solkattu.PatternM)
    let M.Strokes {..} = M.notes
    left_like (f [(2, [k])]) "2 /= realization matras 1"
    equal (f [(2, sd [k])]) (Right ())
    equal (f [(2, su [k, t, k, t])]) (Right ())
    equal (f [(2, [k, t])]) (Right ())

test_strokeMap = do
    let f = fmap (\(Realize.StrokeMap smap) -> Map.toList smap)
            . Realize.strokeMap
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
    let f tala = eFormat . format 80 tala
            . map (Sequence.FNote Sequence.defaultTempo)
        n4 = [k, t, Realize.Space Solkattu.Rest, n]
        M.Strokes {..} = Realize.Note . Realize.stroke <$> M.strokes
        rupaka = Tala.rupaka_fast
    -- Emphasize every 4.
    equal (f rupaka n4) "k t _ n"
    -- Alignment should be ignored.
    equal (f rupaka ([Realize.Alignment 0] <> n4)) "k t _ n"
    equal (f rupaka (n4 <> n4)) "k t _ n k t _ n"
    -- Emphasis works in patterns.
    equal (f rupaka (n4 <> [rpattern 5] <> n4))
        "k t _ n p5--------k t _ n"
    -- Patterns are wrapped properly.
    equal (f rupaka (n4 <> [rpattern 5] <> n4 <> [rpattern 5]))
        "k t _ n p5--------k t _\n\
        \n p5--------"
    -- Emphasize according to the tala.
    let kook = [k, o, o, k]
    equal (f Tala.khanda_chapu (take (5*4) (cycle kook)))
        "k o o k k o o k k o o k k o o k k o o k"

test_format_space = do
    let run = fmap (eFormat . format 80 Tala.adi_tala . fst)
            . kRealize False Tala.adi_tala
    equal (run (Notation.sarvaM 4)) $ Right "========"
    equal (run (Notation.sarvaD 1)) $ Right "========"
    equal (run (Notation.restM 4)) $ Right "_|_ _ _"
    equal (run (Notation.restD 1)) $ Right "_|_ _ _"

tala4 :: Tala.Tala
tala4 = Tala.Tala "tala4" [Tala.O, Tala.O] 0

test_formatRuler = do
    let run = fmap (first (capitalizeEmphasis . format 80 tala4))
            . kRealize False tala4
    let tas nadai n = Dsl.nadai nadai (Dsl.repeat n ta)
    equalT (run (tas 2 8)) $ Right
        ( "X   O   X   O   |\n\
          \K k K k K k K k"
        , ""
        )
    equalT (run (tas 2 16)) $ Right
        ( "X   O   X   O   |\n\
          \K k K k K k K k\n\
          \K k K k K k K k"
        , ""
        )
    equalT (run (tas 3 12)) $ Right
        ( "X:3   O     X     O     |\n\
          \K k k K k k K k k K k k"
        , ""
        )

    equalT (run (tas 2 12 <> tas 3 6)) $ Right
        ( "X   O   X   O   |\n\
          \K k K k K k K k\n\
          \X   O   X:3   O     |\n\
          \K k K k K k k K k k"
        , ""
        )
    -- A final stroke won't cause the ruler to reappear.
    equalT (run (tas 2 16 <> ta)) $ Right
        ( "X   O   X   O   |\n\
          \K k K k K k K k\n\
          \K k K k K k K k K"
        , ""
        )
    equal_fmt (either id id) (fst <$> run (tas 4 2)) $ Right
        "X   |\n\
        \K k"
    equal_fmt (either id id) (fst <$> run (tas 8 8)) $ Right
        "X:4     .       |\n\
        \K k k k k k k k"

equalT :: (CallStack.Stack, Eq a, Show a) => Either Text (Text, a)
    -> Either Text (Text, a) -> IO Bool
equalT = equal_fmt (either id fst)

test_formatLines = do
    let f strokeWidth width tala =
            fmap (extract . Realize.formatLines strokeWidth width tala . fst)
            . kRealize False tala
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

test_formatSymbol = do
    let f = fmap (extract . Realize.formatLines 2 80 tala . fst)
            . kRealize False tala
        extract = map ((\(Realize.Symbol _ b c) -> (b, c)) . snd)
            . head . head
        tala = Tala.rupaka_fast
    let group = Notation.dropM 0
    let tas n = Dsl.repeat n ta
    equal (f (group $ tas 4 <> group (tas 4))) $ Right
        [ (True, [Start]), (False, []), (False, []), (False, [])
        , (True, [Start]), (False, []), (False, []), (False, [End, End])
        ]

test_annotateGroups = do
    let f = map (second (pretty . snd))
            . Realize.annotateGroups
            . Sequence.normalizeSpeed Tala.adi_tala
            . Sequence.flatten
    equal (f (ta <> ki)) [([], "ta"), ([], "ki")]
    equal (f (Notation.group ta <> ki)) [([Start, End], "ta"), ([], "ki")]
    equal (f (Notation.group (ta <> ki))) [([Start], "ta"), ([End], "ki")]
    equal (f (Notation.group (ta <> Notation.group ki)))
        [([Start], "ta"), ([Start, End, End], "ki")]
    equal (f (Notation.group (Notation.group (ta <> ki))))
        [([Start, Start], "ta"), ([End, End], "ki")]

test_formatBreakLines = do
    let run width = fmap (stripEmphasis . format width tala4 . fst)
            . kRealize False tala4
    let tas n = Dsl.repeat n ta
    equal (run 80 (tas 16)) $ Right
        "X:4     O       X       O       |\n\
        \k k k k k k k k k k k k k k k k"
    equal (run 10 (tas 16)) $ Right
        "X:4 O   |\n\
        \kkkkkkkk\n\
        \kkkkkkkk"

test_formatNadaiChange = do
    let f tala realizePatterns =
            fmap (first (stripEmphasis . format 50 tala))
            . kRealize realizePatterns tala
    let sequence = Dsl.su (Dsl.__ <> Dsl.repeat 5 Dsl.p7)
            <> Dsl.nadai 6 (Dsl.tri Dsl.p7)
    let (out, warn) = expect_right $ f Tala.adi_tala True sequence
    equal (Text.lines out)
        [ "0:8     1       2       3       |"
        , "_k_t_knok t knok_t_knok t knok_t"
        -- TODO should be a ruler here
        , "_knok _ t _ k n o k _ t _ k n o k _ t _ k n o"
        ]
    equal warn ""
    -- 0123456701234567012345670123456701234560123450123450123450
    -- 0       1       2       3       4   |  5     6     7     8
    -- _k_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_knok_t_kno

test_formatSpeed = do
    let f width = fmap (capitalizeEmphasis . dropRulers
                . format width Tala.rupaka_fast)
            . realize strokeMap
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

-- * verifyAlignment

test_verifyAlignment = do
    let f = verifyAlignment tdktSmap Tala.adi_tala 0 0
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f []) (Right Nothing)
    equal (f (take 4 tdkt)) $ Right $ Just
        ( 4
        , "should end on sam, actually ends on 1:1, or sam - 7"
        )
    equal (f (take 6 tdkt)) $ Right $ Just
        (6
        , "should end on sam, actually ends on 1:1+1/2, or sam - 6+1/2"
        )
    equal (f (take (8*4) tdkt)) (Right Nothing)
    equal (f (Dsl.speed (-2) $ take 8 tdkt)) (Right Nothing)
    -- Ok to end on sam, even with trailing rests.
    equal (f (Dsl.speed (-2) $ take 9 tdkt <> __ <> __)) (Right Nothing)
    -- But I don't drop rests because sometimes they make it line up.
    equal (f (Dsl.speed (-2) $ take 7 tdkt <> __)) (Right Nothing)

    equal (f (Dsl.speed (-2) $ take 4 tdkt <> Dsl.akshara 4 <> take 4 tdkt))
        (Right Nothing)
    equal (f (take 3 tdkt <> Dsl.akshara 4 <> take 5 tdkt)) $ Right
        (Just (3, "expected akshara 4, but at 1:3/4"))

test_verifyAlignment_eddupu = do
    let f = verifyAlignment tdktSmap Tala.adi_tala
        tdkt = cycle $ ta <> di <> ki <> ta
    equal (f 0 1 (take 8 tdkt)) $ Right $ Just
        (8, "should end on sam+1, actually ends on 1:2, or sam - 6")
    equal (f 0 1 (take 4 tdkt)) $ Right Nothing
    equal (f 0 (-1) (take 4 tdkt)) $ Right $ Just
        (4, "should end on sam-1, actually ends on 1:1, or sam - 7")
    equal (f 0 (-1) (take (4*7) tdkt)) $ Right Nothing

    equal (f 0 0 (take (8*4) tdkt)) $ Right Nothing
    equal (f 1 1 (take (8*4) tdkt)) $ Right Nothing

test_verifyAlignmentNadaiChange = do
    let f = verifyAlignment tdktSmap Tala.adi_tala 0 0
        tdkt = ta <> di <> ki <> ta
    -- Change nadai in the middle of an akshara.
    equal (f (take 2 tdkt <> Dsl.nadai 6 (take 3 tdkt))) $ Right $ Just
        (5, "should end on sam, actually ends on 1:1, or sam - 7")

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

tdktSmap :: Realize.StrokeMap M.Stroke
tdktSmap = expect_right $ Realize.strokeMap
    [ (ta, [k])
    , (di, [t])
    , (ki, [p])
    , (tha, [k])
    , (thom, [o])
    ]
    where M.Strokes {..} = M.notes

verifyAlignment :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> Tala.Tala -> Sequence.Duration -> Sequence.Duration -> Korvai.Sequence
    -> Either Text (Maybe (Int, Text))
verifyAlignment smap tala startOn endOn =
    fmap (Realize.verifyAlignment tala startOn endOn . Sequence.tempoNotes)
        . realize smap

-- * util

realizeN :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Note stroke]
realizeN smap = fmap Sequence.flattenedNotes . realize smap

realize :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [Sequence.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Realized stroke]
realize smap = Realize.formatError
    . Realize.realize Realize.keepPattern (Realize.realizeSollu smap)
    . Sequence.flatten

kRealize :: Bool -> Tala.Tala -> Korvai.Sequence
    -> Either Text ([Korvai.Flat M.Stroke], Text)
kRealize realizePatterns tala =
    head . Korvai.realize Korvai.mridangam realizePatterns
    . Korvai.korvaiInferSections tala mridangam
    . (:[])

strokeMap :: Realize.StrokeMap M.Stroke
strokeMap = expect_right $ Realize.strokeMap
    [ (thom, [o])
    ]
    where M.Strokes {..} = M.notes

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.instMridangam = Dsl.check $
        Realize.instrument [(ta, [M.k M.notes])] M.defaultPatterns
    }

sd, su :: [Sequence.Note g a] -> [Sequence.Note g a]
sd = (:[]) . Sequence.changeSpeed (-1)
su = (:[]) . Sequence.changeSpeed 1

nadai :: Sequence.Nadai -> [Sequence.Note g a] -> Sequence.Note g a
nadai n = Sequence.TempoChange (Sequence.Nadai n)

eFormat :: Text -> Text
eFormat = stripEmphasis . dropRulers

dropRulers :: Text -> Text
dropRulers = Text.strip . Text.unlines . filter (not . isRuler) . Text.lines
    where
    isRuler t = Text.all Char.isDigit (Text.take 1 t)
        || "X" `Text.isPrefixOf` t

-- | Replace emphasis with capitals, so spacing is preserved.
capitalizeEmphasis :: Text -> Text
capitalizeEmphasis =
    TextUtil.mapDelimited True '!' (Text.replace "-" "=" . Text.toUpper)
    . Text.replace "\ESC[0m" "!" . Text.replace "\ESC[1m" "!"

stripEmphasis :: Text -> Text
stripEmphasis = Text.replace "\ESC[0m" "" . Text.replace "\ESC[1m" ""

statePos :: Sequence.State -> (Int, Tala.Akshara, Sequence.Duration)
statePos state =
    ( Sequence.stateAvartanam state
    , Sequence.stateAkshara state
    , Sequence.stateMatra state
    )

format :: Solkattu.Notation stroke => Int -> Tala.Tala
    -> [Sequence.Flat g (Realize.Note stroke)] -> Text
format = Realize.format Nothing
