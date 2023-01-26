-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Realize_test where
import           Prelude hiding ((^))
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Solkattu.Dsl.Mridangam as R
import qualified Solkattu.Dsl.Solkattu as G
import           Solkattu.Dsl.Solkattu
    (__, di, din, ga, ka, ki, na, ta, tang, tha, thom, (^))
import qualified Solkattu.Instrument.Mridangam as M
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import           Solkattu.Solkattu (Note(..), Sollu(..))
import qualified Solkattu.Tala as Tala
import qualified Solkattu.Talas as Talas

import           Global
import           Util.Test


test_realize :: Test
test_realize = do
    let f = eWords . fmap S.flattenedNotes . realizeStrokeMap smap . mconcat
        smap = makeMridangam
            [ (ta <> din, k <> od)
            , (na <> din, n <> od)
            , (ta, t)
            , (din <> __ <> ga, od <> __ <> __)
            ]
            where M.Strokes {..} = M.notes
    equal (f [__, ta, __, __, din]) (Right "_ k _ _ D")
    equal (f [G.p5, __, ta, din]) (Right "k t k n o _ k D")
    equal (f [ta, ta]) (Right "t t")
    equal (f [din, ga]) (Right "D _")
    left_like (f [din, din]) "sequence not found"
    equal (f [G.sarvaM_ 4]) (Right "==4")
    equal (f [din, __, ga]) (Right "D _ _")
    equal (f [din, __, ga, ta, din]) (Right "D _ _ k D")

test_realizeGroups :: Test
test_realizeGroups = do
    let f = eWords . realizeSollu smap
        smap = checkSolluMap
            [ (taka, k <> t)
            , (din, od)
            ]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
    equal (f taka) (Right "k t")

    -- TODO these could also go in Notation_test
    equal (f $ G.dropM 1 taka) (Right "t")
    equal (f $ G.takeM 1 taka) (Right "k")
    equal (f $ G.takeM 2 (taka <> din)) (Right "k t")
    equal (f $ G.dropM 2 (taka <> din)) (Right "D")
    equal (f $ G.dropM 1 $ G.su taka) $ Right ""
    equal (f $ G.su $ G.dropM 1 taka) $ Right "t"

    -- Groups keep finding fragments.
    equal (f $ G.dropM 1 $ taka <> din) (Right "t D")
    equal (f $ G.dropM 2 $ taka <> din) (Right "D")
    equal (f $ G.dropM 3 $ taka <> din) (Right "")
    equal (f $ G.dropM 3 $ taka <> din <> din) (Right "D")
    equal (f $ G.takeM 2 $ din <> taka) (Right "D k")
    equal (f $ G.takeM 1 $ din <> taka) (Right "D")
    equal (f $ G.takeM 0 $ din <> taka) (Right "")
    equal (f $ G.rdropM 1 taka) (Right "k")
    equal (f $ G.rtakeM 1 taka) (Right "t")
    equal (f $ mconcat $ replicate 2 $ G.takeM 1 taka) (Right "k k")

    left_like (f $ ka <> ka) "sequence not found"
    left_like (f $ G.dropM 1 (ka <> ka)) "sequence not found"
    -- I leave the extra 'k' in the output, otherwise 'sequence not found' will
    -- be suppressed by 'group split too long' error.
    left_like (f $ G.dropM 1 $ taka <> G.dropM 1 (ka<>ka))
        "k  t*sequence not found"

    -- With rests.
    equal (f $ G.dropM 1 $ ta <> __ <> ka <> __) (Right "_ t _")

test_realizeGroupsOutput :: Test
test_realizeGroupsOutput = do
    -- Ensure groups are still in the output, and dropped sollus replaced
    -- with strokes.
    let f = extract . realizeSolluMap smap
        smap = checkSolluMap [(taka, k <> t)]
            where M.Strokes {..} = M.notes
        taka = ta <> ka
        extract = fmap $ Text.unwords . map fmt
        fmt (S.FGroup _ g children) =
            pretty g <> "(" <> Text.unwords (map fmt children) <> ")"
        fmt (S.FNote _ n) = pretty n
    equal (f taka) (Right "k t")
    equal (f $ G.dropM 1 taka) (Right "([k], Before)(t)")
    equal (f $ G.rdropM 1 taka) (Right "([t], After)(k)")

test_realizeGroupsNested :: Test
test_realizeGroupsNested = do
    let f = fmap (mconcatMap pretty) . realizeSollu smap
        smap = checkSolluMap [(nakita, mconcat [n, k, t])]
            where M.Strokes {..} = M.notes
        nakita = na <> ki <> ta
    equal (f $ G.reduceTo 1 1 nakita) $ Right $ mconcat
        [ "nkt"
        , "kt"
        , "t"
        ]

    -- nested reduction
    equal (f $ G.reduceTo 2 2 $ G.reduceTo 1 1 nakita) $
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
    equal (f $ G.dropM 4 $ nakita <> G.dropM 1 nakita) $
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
    equal (f $ G.dropM 2 $ G.dropM 1 nakita <> nakita) $ Right "nkt"
    equal (f $ G.dropM 1 $ nakita <> G.dropM 1 nakita) $ Right "ktkt"

test_realizeSarva :: Test
test_realizeSarva = do
    let f = eWords . realizeSollu smap . mconcat
        smap = checkSolluMap
            [ (ta <> din, n <> d)
            , (ta <> ka <> __, mconcat [p, k, __])
            ] where M.Strokes {..} = M.notes
    equal (f []) (Right "")
    equal (f [G.sarvaM (ta <> din) 5]) (Right "n d n d n")
    left_like (f [G.sarvaM (ta <> din <> ta) 5]) "incomplete match"
    equal (f [G.sarvaM (ta <> ka <> __) 5]) (Right "p k _ p k")
    equal (f [G.sarvaM (ta <> din) 5]) (Right "n d n d n")
    -- sarva is relative to sam
    right_equal (f [__, G.sarvaM (ta <> din) 3]) "_ d n d"

test_realizeEmphasis :: Test
test_realizeEmphasis = do
    let f = second (map (fmap pretty)) . realizeSollu smap . mconcat
        smap = checkSolluMap [(ta <> di, G.hv k <> G.lt t)]
            where M.Strokes {..} = M.notes
    equal (f [ta, di]) $ Right
        [ Realize.Note $ Realize.Stroke Realize.Heavy "k"
        , Realize.Note $ Realize.Stroke Realize.Light "t"
        ]

test_realizeTag :: Test
test_realizeTag = do
    let f = eWords . realizeSollu smap
        smap = checkSolluMap
            [ (ta <> ta, p <> p)
            , (ta, k)
            , (1^ta, t)
            ] where M.Strokes {..} = M.notes
    equal (f ta) (Right "k")
    equal (f (ta <> ta)) (Right "p p")
    -- Having a tag is more important than a longer match.
    equal (f (1^ta <> ta)) (Right "t k")
    equal (f (2^ta)) (Right "k")

test_realizePatterns :: Test
test_realizePatterns = do
    let f pmap = Realize.formatError . fst
            . Realize.realize_ (Realize.realizePattern pmap)
                (Realize.realizeSollu solluMap) adiAksharas
            . S.flatten . S.toList
    let eStrokes = eWords . fmap S.flattenedNotes
    equal (eStrokes $ f (M.families567 !! 0) G.p5)
        (Right "k t k n o")
    equal (eStrokes $ f (M.families567 !! 1) G.p5)
        (Right "k _ t _ k _ k t o _")
    -- This ensures that 'Realize.realize' fixes the FGroup count if
    -- realizePatterns changes it.
    equal (eStrokes $ f M.defaultPatterns $ G.rdropM 0 $ G.sd G.p5)
        (Right "k t k n o")
    left_like (f (M.families567 !! 0) (G.pat 3)) "no pattern for 3p"

test_patterns :: Test
test_patterns = do
    let f = second (const ()) . Realize.patternMap . map (second S.fromList)
            . solkattuToRealize . map (first Solkattu.pattern)
    let M.Strokes {..} = M.notes
    left_like (f [(2, k)]) "2 /= realization matras 1"
    right_equal (f [(2, G.sd k)]) ()
    right_equal (f [(2, G.su (k <> t <> k <> t))]) ()
    right_equal (f [(2, k <> t)]) ()

test_solluMap :: Test
test_solluMap = do
    let f = fmap (\(Realize.SolluMap smap) -> Map.toList smap) . makeSolluMap
    let M.Strokes {..} = M.notes
    right_equal (f []) []
    right_equal (f [(ta <> di, k <> t)])
        [ ( (Nothing, [Ta, Di])
          , map (Just . Realize.stroke) [M.Valantalai M.Ki, M.Valantalai M.Ta]
          )
        ]
    -- Last one wins.
    right_equal (f [(ta, k), (ta, t)])
        [((Nothing, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ta))])]
    equal (f [(1^ta, k)]) $ Right
        [((Just 1, [Ta]), [Just (Realize.stroke (M.Valantalai M.Ki))])]
    left_like (f [(ta <> di <> ta, k)]) "more sollus than strokes at di.ta"
    left_like (f [(ta, mconcat [k, t, k])]) "more strokes than sollus at tk"
    equal (length <$> f [(tang <> ga, u <> __)]) (Right 1)
    left_like (f [(tang <> __, k <> t)]) "rest sollu given non-rest stroke"
    left_like (f [(ta <> G.pat 5, k)]) "only have plain sollus"

test_checkD :: Test
test_checkD = do
    let f = prettyStrokes . realizeM Tala.any_beats
    right_equal (f (G.checkD 4 (R.k <> R.t)))
        ("k t", [Realize.Warning (Just 0) "expected 4 aksharas, but was 1/2"])
    right_equal (f (G.checkD 1 (R.k <> R.t <> R.p <> R.k)))
        ("k t p k", [])

-- * checkAlignment

test_checkAlignment :: Test
test_checkAlignment = do
    let f = checkAlignment tdktSmap adiAksharas 0 0
        tdkt = cycle [ta, di, ki, ta]
        mtake n = mconcat . take n
    equal (f mempty) (Right Nothing)
    equal (f (mtake 4 tdkt)) $ Right $ Just $ Realize.Warning Nothing
        "should end on sam, actually ends on 1:1, or sam - 7"
    equal (f (mtake 6 tdkt)) $ Right $ Just $ Realize.Warning Nothing
        "should end on sam, actually ends on 1:1+1/2, or sam - 6+1/2"
    equal (f (mtake (8*4) tdkt)) (Right Nothing)
    equal (f (G.speed (-2) $ mtake 8 tdkt)) (Right Nothing)
    -- Ok to end on sam, even with trailing rests.
    equal (f (G.speed (-2) $ mtake 9 tdkt <> __ <> __)) (Right Nothing)
    -- But I don't drop rests because sometimes they make it line up.
    equal (f (G.speed (-2) $ mtake 7 tdkt <> __)) (Right Nothing)

    equal (f (G.speed (-2) $ mtake 4 tdkt <> G.akshara 4 <> mtake 4 tdkt))
        (Right Nothing)
    equal (f (mtake 3 tdkt <> G.akshara 4 <> mtake 5 tdkt)) $ Right $ Just $
        Realize.Warning (Just 3) "expected akshara 4, but at 1:3/4"

test_checkAlignment_eddupu :: Test
test_checkAlignment_eddupu = do
    let f = checkAlignment tdktSmap adiAksharas
        tdkt = cycle [ta, di, ki, ta]
        mtake n = mconcat . take n
    equal (f 0 1 (mtake 8 tdkt)) $ Right $ Just $ Realize.Warning
        Nothing "should end on sam+1, actually ends on 1:2, or sam - 6"
    equal (f 0 1 (mtake 4 tdkt)) $ Right Nothing
    equal (f 0 (-1) (mtake 4 tdkt)) $ Right $ Just $ Realize.Warning
        Nothing "should end on sam-1, actually ends on 1:1, or sam - 7"
    equal (f 0 (-1) (mtake (4*7) tdkt)) $ Right Nothing

    equal (f 0 0 (mtake (8*4) tdkt)) $ Right Nothing
    equal (f 1 1 (mtake (8*4) tdkt)) $ Right Nothing

test_checkAlignmentNadaiChange :: Test
test_checkAlignmentNadaiChange = do
    let f = checkAlignment tdktSmap adiAksharas 0 0
    -- Change nadai in the middle of an akshara.
    right_equal (f (ta <> di <> G.nadai 6 (mconcat [ta, di, ki]))) $ Just $
        Realize.Warning Nothing
            "should end on sam, actually ends on 1:1, or sam - 7"

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
    let sequence p7 = G.nadai 8 (__ <> G.repeat 5 p7) <> G.nadai 6 (G.tri p7)
    equal (f (sequence (ta <> __ <> di <> __ <> ki <> tha <> thom)))
        (Right Nothing)
    equal (f (sequence G.p7)) (Right Nothing)

tdktSmap :: Realize.StrokeMap M.Stroke
tdktSmap = makeMridangam
    [ (ta, k)
    , (di, t)
    , (ki, p)
    , (tha, k)
    , (thom, o)
    ]
    where M.Strokes {..} = M.notes

checkAlignment :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> Tala.Akshara -> S.Duration -> S.Duration -> Korvai.Sequence
    -> Either Text (Maybe Realize.Warning)
checkAlignment smap talaAksharas startOn endOn =
    fmap (Realize.checkAlignment talaAksharas startOn endOn . S.tempoNotes)
        . realizeStrokeMap smap

-- * util

adiAksharas :: Tala.Akshara
adiAksharas = Tala.tala_aksharas Tala.adi_tala

eWords :: Pretty b => Either a [b] -> Either a Text
eWords = fmap (Text.unwords . map pretty)

prettyStrokes :: Either err ([Korvai.Flat M.Stroke], [Realize.Warning])
    -> Either err (Text, [Realize.Warning])
prettyStrokes = second (first (Text.unwords . map pretty . S.flattenedNotes))

-- | Realize a mridangam stroke score.
realizeM :: Tala.Tala -> Korvai.SequenceT (Realize.Stroke M.Stroke)
    -> Either Korvai.Error ([Korvai.Flat M.Stroke], [Realize.Warning])
realizeM tala =
    Korvai.realizeSection (Talas.Carnatic tala) Realize.realizeStroke mempty id
    . Korvai.section

checkSolluMap :: CallStack.Stack =>
    [ ( S.Sequence g (Note Sollu)
      , S.Sequence g (Note (Realize.Stroke M.Stroke))
      )
    ] -> Realize.SolluMap Solkattu.Sollu M.Stroke
checkSolluMap = expect_right . makeSolluMap

makeSolluMap ::
    [ ( S.Sequence g (Note Sollu)
      , S.Sequence g (Note (Realize.Stroke M.Stroke))
      )
    ] -> Either Text (Realize.SolluMap Solkattu.Sollu M.Stroke)
makeSolluMap = fmap fst . Realize.solluMap . solkattuToRealize

makeMridangam :: G.StrokeMap M.Stroke -> Realize.StrokeMap M.Stroke
makeMridangam = expect_right . Korvai.smapMridangam . G.makeMridangam0

-- | Realize sollus.  Since this doesn't go through 'Korvai.realizeSection',
-- it omits the post-realize checks, so no 'Realize.Warning's.
realizeSollu :: Solkattu.Notation stroke
    => Realize.SolluMap Solkattu.Sollu stroke
    -> S.Sequence Solkattu.Group (Note Sollu)
    -> Either Text [Realize.Note stroke]
realizeSollu smap = fmap S.flattenedNotes . realizeSolluMap smap

realizeSolluMap :: Solkattu.Notation stroke
    => Realize.SolluMap Solkattu.Sollu stroke
    -> S.Sequence Solkattu.Group (Note Sollu)
    -> Either Text [Realize.Realized stroke]
realizeSolluMap solluMap = realizeStrokeMap smap
    where
    smap = Realize.StrokeMap
        { smapSolluMap = solluMap
        , smapSolluShadows = []
        , smapPatternMap = mempty
        }

realizeStrokeMap :: Solkattu.Notation stroke => Realize.StrokeMap stroke
    -> S.Sequence Solkattu.Group (Note Sollu)
    -> Either Text [Realize.Realized stroke]
realizeStrokeMap smap =
    Realize.formatError . fst
    . Realize.realize smap (Realize.realizeSollu (Realize.smapSolluMap smap))
        adiAksharas
    . S.flatten . S.toList
    . fmap (fmap Realize.stroke)

solluMap :: Realize.SolluMap Solkattu.Sollu M.Stroke
solluMap = checkSolluMap
    [ (thom, o)
    ]
    where M.Strokes {..} = M.notes

mridangam :: Korvai.StrokeMaps
mridangam = mempty
    { Korvai.smapMridangam = Realize.strokeMap M.defaultPatterns [(ta, k)]
    }
    where M.Strokes {..} = M.notes

solkattuToRealize :: [(a, S.Sequence g (Solkattu.Note (Realize.Stroke stroke)))]
    -> [(a, [S.Note () (Realize.Note stroke)])]
solkattuToRealize = expect_right . mapM (traverse Realize.solkattuToRealize)
