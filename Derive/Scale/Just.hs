-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A version of a just intonation diatonic scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.Raga as Raga
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = simpleScales ++ makeScales

simpleScales :: [Scale.Definition]
simpleScales = map Scale.Simple
    [ Scales.addDoc "7-note just scale." $
        JustScales.makeScale "just" (scaleMap TheoryFormat.absoluteC)
            justDoc docFields
    , Scales.addDoc "7-note just scale." $
        JustScales.makeScale "just-r"
            (scaleMap (TheoryFormat.sargam relativeFmt)) justDoc docFields
    ]
    where
    relativeFmt = JustScales.makeRelativeFmt keys defaultKey

makeScales :: [Scale.Definition]
makeScales =
    [ scaleMakeJust "make-just7" TheoryFormat.absoluteC
    , scaleMakeJust "make-just7-r" (TheoryFormat.sargam relativeFmt)
    ]
    where
    relativeFmt = JustScales.makeRelativeFmt mempty defaultKey
    defaultKey = JustScales.Key
        { keyTonic = 0
        , keyRatios = Map.fromList []
        }

justDoc :: Doc.Doc
justDoc =
    "7-note scales in just intonation.\
    \\nThey are fundamentally 7 note scales because they use a A-G or SRGMPDN.\
    \ While they support sharps and flats and have a piano-style layout,\
    \ accidentals are implemented as simple ratio offsets from base pitch.\
    \ Extending to more flexible notions of just intonation would require\
    \ generalizing the input mapping and pitch notation.\
    \\nKeys look like `c-maj`, where `c` is the tonic and `maj` selects\
    \ the ratios to use. For absolute notation, the tonic determines where\
    \ the scale starts, while for relative notation, the tonic determines\
    \ only which MIDI key maps to the first scale degree. So for the ASCII kbd\
    \ where the input is also relative, the tonic is irrelevant."

docFields :: [(Doc.Doc, Doc.Doc)]
docFields =
    [ (Doc.literal name,
        Doc.Doc $ JustScales.showRatios (JustScales.keyRatios key))
    | (name, key) <- ChromaticScales.groupTonicMode (Map.toList keys)
    ]

scaleMap :: TheoryFormat.Format -> JustScales.ScaleMap
scaleMap = JustScales.scaleMap keys defaultKey (Just defaultTuning)

defaultKey :: JustScales.Key
Just defaultKey = Map.lookup (Pitch.Key "c-maj") keys

keys :: Map Pitch.Key JustScales.Key
keys = JustScales.makeKeys TheoryFormat.absoluteCDegrees keyRatios

keyRatios :: [(Text, [(JustScales.Tuning, JustScales.Ratios)])]
keyRatios =
    [ (name, map (second (select is)) (Map.toList tunings))
    | (name, is) <- namedIntervals
    ]

namedIntervals :: [(Text, [Pitch.Semi])]
namedIntervals = map (second (take 7)) $ zip names $ List.tails $
    cycle Theory.pianoIntervals
    where
    -- The names are the same as in Twelve.
    names = ["maj", "dorian", "phrygian", "lydian", "mixolydian", "min",
        "locrian"]

select :: [Int] -> Vector.Vector a -> Vector.Vector a
select intervals v =
    Vector.unfoldrN (length intervals) make (scanl (+) 0 intervals)
    where
    make (i:is) = Just (v Vector.! i, is)
    make [] = Nothing

tunings :: Map JustScales.Tuning JustScales.Ratios
tunings = Map.fromList $ map (second Vector.fromList)
    -- 5-limit would be more natural than limit-5, but I'd need quotes then.
    [ ("limit-5",
        [1, 16/15, 9/8, 6/5, 5/4, 4/3, 45/32, 3/2, 8/5, 5/3, 9/5, 15/8])
    , ("limit-7",
        [1, 15/14, 8/7, 6/5, 5/4, 4/3, 7/5, 3/2, 8/5, 5/3, 7/4, 15/8])
    ]
    -- 11+ limit adds intervals which get increasingly far from the 7 note
    -- framework, so I'd need some other way to name them.

defaultTuning :: JustScales.Tuning
defaultTuning = "limit-5"

{-
    c   db     d    eb   e    f    f#     g    ab   a    bb   b
    [1, 16/15, 9/8, 6/5, 5/4, 4/3, 45/32, 3/2, 8/5, 5/3, 9/5, 15/8]

    Ptolemy's intense diatonic scale.
    I, IV, and V are in-tune major triads of 4:5:6.
    But ii and vii dim not very ideal.
    [1,        9/8,      5/4, 4/3,        3/2,      5/3,      15/8]

    i, iv, and v are in-tune minor triads of 10:12:15.
    But ii dim and VII are out.
    [1,        9/8, 6/5,      4/3,        3/2, 8/5,      9/5]
-}


-- * make just

-- | Make a 7 note just scale with custom ratios or intervals.
scaleMakeJust :: Pitch.ScaleId -> TheoryFormat.Format -> Scale.Definition
scaleMakeJust scaleId fmt =
    Scale.Make scaleId (TheoryFormat.fmtPattern fmt, callDoc)
        (makeJust scaleId fmt)
    where
    callDoc = Scales.annotateCallDoc Scales.standardTransposers
        doc [] JustScales.defaultCallDoc
    doc = "Set " <> ShowVal.doc justRatios <> " to make a custom scale, and "
        <> ShowVal.doc justIntervals <> " to select from it. The intervals\
        \ default to replicate 7 1, and the number of ratios should be\
        \ sum intervals - 1 (the initial 1/1 is implicit).\
        \ Ratios can be a list, or one of " <> literals (Map.keys tunings)
        <> ", and intervals can be a list or one of "
        <> literals (map fst namedIntervals
            ++ map fst Raga.melakartaIntervals)
        <> "\n\n" <> justDoc
    literals = Doc.commas . map Doc.literal

allNamedIntervals :: Map Text [Pitch.Semi]
allNamedIntervals = Map.fromList $ namedIntervals ++ Raga.melakartaIntervals

justRatios :: Env.Key
justRatios = "just-ratios"

justIntervals :: Env.Key
justIntervals = "just-intervals"

makeJust :: Pitch.ScaleId -> TheoryFormat.Format
    -> Env.Environ -> Scale.LookupScale
    -> Either DeriveT.PitchError Scale.Scale
makeJust scaleId fmt env _ = do
    intervals <- parseIntervals env
    ratios <- parseRatios intervals env
    let defaultKey = JustScales.Key
            { keyTonic = 0
            , keyRatios = Map.fromList [("", ratios)]
            }
    let smap = JustScales.scaleMap Map.empty defaultKey Nothing fmt
    pure $ JustScales.makeScale scaleId smap "unused doc" []

parseIntervals :: Env.Environ -> Either DeriveT.PitchError [Pitch.Semi]
parseIntervals =
    Scales.readEnviron_ parse (Just (Right (replicate 7 1))) justIntervals
    where
    parse (Left xs) = Right xs
    parse (Right sym) = maybe
        (Left $ Just $ "not one of: " <> pretty (Map.keys allNamedIntervals))
        Right (Map.lookup sym allNamedIntervals)

parseRatios :: [Pitch.Semi] -> Env.Environ
    -> Either DeriveT.PitchError JustScales.Ratios
parseRatios intervals = Scales.readEnviron_ parse Nothing justRatios
    where
    parse (Right sym) = maybe
        (Left $ Just $ "not one of: " <> pretty (Map.keys tunings))
        check (Map.lookup sym tunings)
    parse (Left ratios) = check $ Vector.fromList (1:ratios)
    check ratios
        | Vector.length ratios == Num.sum intervals =
            Right $ select intervals ratios
        | otherwise = Left $ Just $
            "length should be sum " <> pretty intervals <> " - 1, but was "
            <> pretty (Vector.length ratios - 1)
        -- I don't subtract 1 because I already put 1/1 on.
