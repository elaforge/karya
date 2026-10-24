-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.Check_test where
import qualified Data.Text as Text

import qualified Util.Logger as Logger
import qualified Util.Test.Testing as Testing
import qualified Derive.JScore.Check as Check
import qualified Derive.JScore.Parse as Parse
import qualified Derive.JScore.T as T
import           Derive.JScore.T (Pitch(..), PitchClass(..))

import           Global
import           Util.Test.Global


test_format_score :: Test
test_format_score = do
    let f extract = fmap (eBlock extract) . formatScore
            . ("%irama=lancar\n%laras=slendro-manyura\n"<>)
            -- Make sure there is a valid Meta.
    let eNotes = mconcatMap (pretty . T.mapNote T.notePitch)
    -- normalize_name
    rightEqual (f T.blockNames "1235 kk [ > 1235 ]") [["kutuk-kuning"]]
    leftLike (f T.blockNames "1235 zz [ > 1235 ]") "unknown name"
    -- normalize_hands
    rightEqual (f (map eNotes . T.blockTracks) "1235 [ > 1235 > .5.3.2.1 ]")
        [["01.02.03.05.", ".05.03.02.01"]]
    leftLike (f id "1235 [ > 3333 > 1111 | 2222 ]")
        "left hand with no right hand"
    leftLike (f id "1235 [ > 1235 > 12345 ]") "not a power of 2: 5"
    -- check_range
    leftLike (f id "%instrument = gender-panerus\n1235 [ > ,1656 ]")
        "out of range for GenderPanerus: -25"
    leftLike (f id "%instrument = gender-panerus\n1235 [ > '1353 ]")
        "out of range for GenderPanerus: 15"
    leftLike
        (f id "%instrument = gender-panerus\n%laras = pelog-lima\n\
            \1235 [ > 1234 ]")
        "GenderPanerus doesn't have 4"
    leftLike
        (f id "%instrument = gender-panerus\n%laras = pelog-lima\n\
            \1235 [ > 1271 ]")
        "PelogLima doesn't have 7"
    -- check_hands
    rightEqual (f (const ()) "1235 [ > 2222 > .1.1.1.1 ]") ([()])
    leftLike (f (const ()) "1235 [ > 2212 > 1.1.1.1. ]") "left hand >= right"

eBlock :: (block -> a) -> [T.Toplevel block] -> [a]
eBlock f = mapMaybe $ \case
    T.BlockDefinition b -> Just $ f b
    _ -> Nothing

formatScore :: Text -> Either Text [T.Toplevel Check.Block]
formatScore source = case Parse.parseScore source of
    Left err -> Left err
    Right score
        | null errs -> Right $ map snd toplevels
        | otherwise -> Left $ Text.unwords $ map (T.showError "fn" source) errs
        where
        (T.Score toplevels, errs) = Logger.runId $ Check.formatScore score

test_infer_octave :: Test
test_infer_octave = do
    let f = Check.inferOctave
    let p = T.ParsedPitch
    equal (f (1, Just P1) (p 0 P2)) (Pitch 1 P2)
    equal (f (1, Just P1) (p 0 P7)) (Pitch 0 P7)
    -- First ' forces upward motion.
    equal (f (1, Just P1) (p 1 P2)) (Pitch 1 P2)
    equal (f (1, Just P1) (p 1 P7)) (Pitch 1 P7)
    -- Additional 's add octaves.
    equal (f (1, Just P1) (p 2 P2)) (Pitch 2 P2)
    -- Same for ,
    equal (f (1, Just P1) (p (-1) P2)) (Pitch 0 P2)
    equal (f (1, Just P1) (p (-1) P7)) (Pitch 0 P7)
    equal (f (1, Just P1) (p (-2) P2)) (Pitch (-1) P2)

test_resolve_pitch :: Test
test_resolve_pitch = do
    let f = fmap (map extract) . resolveTokens Check.BiasStart
        extract = T.notePitch . snd
    rightEqual (f "1471")
        [Pitch 3 P1, Pitch 3 P4, Pitch 3 P7, Pitch 4 P1]
    rightEqual (f "11") [Pitch 3 P1, Pitch 3 P1]
    rightEqual (f "147,1") [Pitch 3 P1, Pitch 3 P4, Pitch 3 P7, Pitch 3 P1]

test_resolve_duration_bias_start :: Test
test_resolve_duration_bias_start = do
    let f = fmap (map extract) . resolveTokens Check.BiasStart
        extract (t, n) = (t, T.noteDuration n)
    rightEqual (f "1234") [(0, 1/4), (1/4, 1/4), (2/4, 1/4), (3/4, 1/4)]
    rightEqual (f ".1.2.3.4")
        [(1/8, 1/4), (3/8, 1/4), (5/8, 1/4), (7/8, 1/8)]
    rightEqual (f "1.2.3.4.")
        [(0, 1/4), (1/4, 1/4), (2/4, 1/4), (3/4, 1/4)]
    rightEqual (f "1") [(0, 1)]
    rightEqual (f "1 | 23") [(0, 1), (1, 1/2), (1+1/2, 1/2)]
    rightEqual (f "12..") [(0/4, 1/4), (1/4, 3/4)]
    rightEqual (f "12_.") [(0/4, 1/4), (1/4, 1/4)]
    rightEqual (f "1_.2") [(0/4, 1/4), (3/4, 1/4)]

test_resolve_duration_bias_end :: Test
test_resolve_duration_bias_end = do
    let f = fmap (map extract) . resolveTokens Check.BiasEnd
        extract (t, n) = (t, T.noteDuration n)
    rightEqual (f "1234") [(1/4, 1/4), (2/4, 1/4), (3/4, 1/4), (4/4, 0)]
    rightEqual (f ".1.2.3.4") [(1/4, 1/4), (2/4, 1/4), (3/4, 1/4), (4/4, 0)]
    rightEqual (f "1.2.3.4.")
        [(1/8, 1/4), (3/8, 1/4), (5/8, 1/4), (7/8, 1/8)]
    rightEqual (f "1") [(1, 0)]
    rightEqual (f "1 | 23") [(1, 1/2), (1+1/2, 1/2), (2, 0)]
    rightEqual (f "12..") [(1/4, 1/4), (2/4, 2/4)]
    rightEqual (f "12_.") [(1/4, 1/4), (2/4, 1/4)]
    rightEqual (f "1_.2") [(1/4, 1/4), (4/4, 0)]

test_infer_rests :: Test
test_infer_rests = do
    let f bias = fmap (map extract) . resolveTokens bias
        extract (t, n) = (t * 8, T.noteDuration n * 8)
    -- Last 6 is actually after the 4/4, it belongs to the next measure.
    -- 1st 2 should be at 1/4 though.
    -- This means the beats still have to be aligned at the ends?
    let expected end =
            [ (2, 2), (4, 1), (5, 1), (6, 2), (8, 1), (9, 1)
            , (10, 2), (12, 2), (14, 2), (16, end)
            ]
    let expectedS = map (first (subtract 2)) (expected 2)
        expectedE = expected 0
    {-
        should be 4 beats:
        1 2 3 4
        2 126 56

        Can't have notes before 2 because it has to be from previous measure.

        0  1  2  3  4
           2. 12 6. 56 | 1
           2  12 6  56 | 1

        0  1  2  3  4    1
          .2 .1 26 .5 | 61
    -}
    rightEqual (f Check.BiasStart "2 126 56 | 3 5 3 5") expectedS
    rightEqual (f Check.BiasStart "2. 12 6. 56 | 3 5 3 5") expectedS

    rightEqual (f Check.BiasEnd ".2 .1 26 .5 | 63 5 3 5") expectedE
    rightEqual (f Check.BiasEnd "2 126 5 | 63 5 3 5") expectedE

    -- This is .235, even though it looks misleading.
    -- The problem is rests are only inferred when it's not already a power of
    -- 2.
    rightEqual (const () <$> f Check.BiasStart ". 235") ()
    rightEqual (const () <$> f Check.BiasStart "12345 6") ()

err :: Int -> Text -> T.Error
err pos = T.err (T.Pos pos)

parseTokens :: Text -> [T.ParsedToken]
parseTokens = Testing.expectRight . fmap T.trackTokens
    . Parse.parseText . ("> "<>)

resolveTokens :: Check.Bias -> Text
    -> Either [T.Error] [(T.Time, T.Note Pitch T.Time)]
resolveTokens bias source
    | null errs = Right lines
    | otherwise = Left errs
    where
    (lines, errs) = Logger.runId $ Check.resolveTokens meta bias $
        parseTokens source
    meta = Check.Meta
        { mLaras = T.SlendroManyura
        -- Dadi is written 4 bars / gatra, so this gets back to 1 gatra = 1t
        , mIrama = T.Dadi
        , mInstrument = T.GenderBarung
        }
