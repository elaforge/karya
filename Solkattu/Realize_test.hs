-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Realize_test where
import Prelude hiding ((^))
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Solkattu.Dsl as Dsl
import Solkattu.Dsl ((^), __)
import Solkattu.SolkattuGlobal (ta, di, ki, tha, thom, tang, ga, din, na, ka)
import qualified Solkattu.Instrument.Mridangam as M
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Notation as Notation
import Solkattu.Notation (takeM, dropM, rdropM)
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Solkattu.SolkattuGlobal as SolkattuGlobal
import qualified Solkattu.Tala as Tala

import Global
import Util.Test


test_realize = do
    let f = eWords . fmap S.flattenedNotes . realizeSmap smap . mconcat
        smap = makeMridangam
            [ (ta <> din, k <> od)
            , (na <> din, n <> od)
            , (ta, t)
            , (din <> __ <> ga, od <> __ <> __)
            ]
            where M.Strokes {..} = M.notes
    equal (f [__, ta, __, __, din]) (Right "_ k _ _ D")
    equal (f [Dsl.p5, __, ta, din]) (Right "k t k n o _ k D")
    equal (f [ta, ta]) (Right "t t")
    equal (f [din, ga]) (Right "D _")
    left_like (f [din, din]) "sequence not found"
    equal (f [Notation.sarvaM_ 4]) (Right "==4")
    equal (f [din, __, ga]) (Right "D _ _")
    equal (f [din, __, ga, ta, din]) (Right "D _ _ k D")

test_realizeGroups = do
    let f = eWords . realizeN smap
        smap = checkSolluMap
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

test_realizeGroupsOutput = do
    -- Ensure groups are still in the output, and dropped sollus replaced
    -- with strokes.
    let f = extract . realizeSolluMap smap
        smap = checkSolluMap [(taka, [k, t])]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
        extract = fmap $ Text.unwords . map fmt
        fmt (S.FGroup _ g children) =
            pretty g <> "(" <> Text.unwords (map fmt children) <> ")"
        fmt (S.FNote _ n) = pretty n
    equal (f taka) (Right "k t")
    equal (f $ dropM 1 taka) (Right "([k], Before)(t)")
    equal (f $ rdropM 1 taka) (Right "([t], After)(k)")

test_realizeGroupsNested = do
    let f = fmap (mconcatMap pretty) . realizeN smap
        smap = checkSolluMap [(nakita, [n, k, t])]
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

test_realizeSarva = do
    let f = eWords . realizeN smap . mconcat
        smap = checkSolluMap
            [ (ta <> din, [n, d])
            ] where M.Strokes {..} = M.notes
        sarvaM = Notation.sarvaM
    equal (f []) (Right "")
    equal (f [sarvaM (ta <> din) 5]) (Right "n d n d n")
    left_like (f [sarvaM (ta <> din <> ta) 5]) "incomplete match"

eWords :: Pretty b => Either a [b] -> Either a Text
eWords = fmap (Text.unwords . map pretty)

test_realizeEmphasis = do
    let f = second (map (fmap pretty)) . realizeN smap . mconcat
        smap = checkSolluMap [(ta <> di, [Dsl.hv k, Dsl.lt t])]
            where M.Strokes {..} = M.notes
    equal (f [ta, di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realizeTag = do
    let f = eWords . realizeN smap
        smap = checkSolluMap
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

pattern :: S.Matra -> Solkattu.Note stroke
pattern = Solkattu.Pattern . Solkattu.pattern

test_realizePatterns = do
    let f pmap seq = Realize.formatError $ fst $
            Realize.realize_ (Realize.realizePattern pmap)
                (Realize.realizeSollu solluMap) (S.flatten seq)
    let eStrokes = eWords . fmap S.flattenedNotes
    equal (eStrokes $ f (M.families567 !! 0) Dsl.p5)
        (Right "k t k n o")
    equal (eStrokes $ f (M.families567 !! 1) Dsl.p5)
        (Right "k _ t _ k _ k t o _")
    -- This ensures that 'Realize.realize' fixes the FGroup count if
    -- realizePatterns changes it.
    equal (eStrokes $ f M.defaultPatterns $ rdropM 0 $ sd Dsl.p5)
        (Right "k t k n o")
    left_like (f (M.families567 !! 0) (Dsl.pat 3)) "no pattern for 3p"

test_patterns = do
    let f = second (const ()) . Realize.patternMap . solkattuToRealize
            . map (first Solkattu.pattern)
    let M.Strokes {..} = M.notes
    left_like (f [(2, k)]) "2 /= realization matras 1"
    equal (f [(2, sd k)]) (Right ())
    equal (f [(2, su (k <> t <> k <> t))]) (Right ())
    equal (f [(2, k <> t)]) (Right ())

test_solluMap = do
    let f = fmap (\(Realize.SolluMap smap) -> Map.toList smap) . makeSolluMap
    let M.Strokes {..} = M.notes
    equal (f []) (Right [])
    equal (f [(ta <> di, [k, t])]) $ Right
        [ ( (Nothing, [Ta, Di])
          , map (Just . Realize.stroke) [M.Valantalai M.Ki, M.Valantalai M.Ta]
          )
        ]
    -- Last one wins.
    equal (f [(ta, [k]), (ta, [t])]) $ Right
        [((Nothing, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ta))])]
    equal (f [(1^ta, [k])]) $ Right
        [((Just 1, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ki))])]
    left_like (f [(ta <> di <> ta, [k])]) "more sollus than strokes at di.ta"
    left_like (f [(ta, [k, t, k])]) "more strokes than sollus at tk"
    equal (length <$> f [(tang <> ga, [u, __])]) (Right 1)
    left_like (f [(tang <> __, [k, t])]) "rest sollu given non-rest stroke"
    left_like (f [(ta <> [S.Note $ pattern 5], [k])]) "only have plain sollus"

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
tdktSmap = makeMridangam
    [ (ta, k)
    , (di, t)
    , (ki, p)
    , (tha, k)
    , (thom, o)
    ]
    where M.Strokes {..} = M.notes

verifyAlignment :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> Tala.Tala -> S.Duration -> S.Duration -> Korvai.Sequence
    -> Either Text (Maybe (Int, Text))
verifyAlignment smap tala startOn endOn =
    fmap (Realize.verifyAlignment tala startOn endOn . S.tempoNotes)
        . realizeSmap smap

-- * util

checkSolluMap :: CallStack.Stack =>
    [ ( [S.Note g (Note Sollu)]
      , [[S.Note g (Note (Realize.Stroke M.Stroke))]]
      )
    ] -> Realize.SolluMap M.Stroke
checkSolluMap = expect_right . makeSolluMap

makeSolluMap ::
    [ ( [S.Note g (Note Sollu)]
      , [[S.Note g (Note (Realize.Stroke M.Stroke))]]
      )
    ] -> Either Text (Realize.SolluMap M.Stroke)
makeSolluMap =
    fmap fst . Realize.solluMap . solkattuToRealize . map (second mconcat)

makeMridangam :: SolkattuGlobal.StrokeMap M.Stroke -> Realize.StrokeMap M.Stroke
makeMridangam = expect_right . Korvai.smapMridangam
    . SolkattuGlobal.makeMridangam0

-- TODO better name
realizeN :: Solkattu.Notation stroke => Realize.SolluMap stroke
    -> [S.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Note stroke]
realizeN smap = fmap S.flattenedNotes . realizeSolluMap smap

realizeSolluMap :: Solkattu.Notation stroke => Realize.SolluMap stroke
    -> [S.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Realized stroke]
realizeSolluMap solluMap = realizeSmap smap
    where
    smap = Realize.StrokeMap
        { smapSolluMap = solluMap
        , smapSolluShadows = []
        , smapPatternMap = mempty
        }

realizeSmap :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> [S.Note Solkattu.Group (Note Sollu)]
    -> Either Text [Realize.Realized stroke]
realizeSmap smap =
    Realize.formatError . fst
    . Realize.realize smap (Realize.realizeSollu (Realize.smapSolluMap smap))
    . S.flatten

solluMap :: Realize.SolluMap M.Stroke
solluMap = checkSolluMap
    [ (thom, [o])
    ]
    where M.Strokes {..} = M.notes

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.smapMridangam = Realize.strokeMap M.defaultPatterns [(ta, k)]
    }
    where M.Strokes {..} = M.notes

sd, su :: [S.Note g a] -> [S.Note g a]
sd = (:[]) . S.changeSpeed (-1)
su = (:[]) . S.changeSpeed 1

nadai :: S.Nadai -> [S.Note g a] -> S.Note g a
nadai n = S.TempoChange (S.Nadai n)

solkattuToRealize :: [(a, [(S.Note g (Solkattu.Note (Realize.Stroke stroke)))])]
    -> [(a, [S.Note () (Realize.Note stroke)])]
solkattuToRealize = expect_right . mapM (traverse Realize.solkattuToRealize)
