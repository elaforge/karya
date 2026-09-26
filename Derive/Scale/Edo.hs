-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Equal divisions of the octave.
module Derive.Scale.Edo where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = [make "edo"]

make :: Pitch.ScaleId -> Scale.Definition
make scaleId = Scale.Make scaleId (pattern, callDoc) (makeEdo scaleId)
    where
    pattern = TheoryFormat.fmtPattern $ TheoryFormat.letters 25
    callDoc = Scales.annotateCallDoc Scales.standardTransposers
        doc [] Scales.defaultScaleDegreeDoc
    doc = "Construct a scale from equal divisions of the octave.\
        \ The number of divisions are given by " <> ShowVal.doc edoDivisions
        <> ". The intervals that correspond to diatonic notes are from "
        <> ShowVal.doc edoIntervals <> ", which defaults to all 1s.\
        \ Because the diatonic intervals are given explicitly, there are no\
        \ keys, and because the notes per octave is variable, diatonic notes\
        \ start from A, rather than the conventional C."

makeEdo :: Pitch.ScaleId -> Env.Environ -> Scale.LookupScale
    -> Either DeriveT.PitchError Scale.Scale
makeEdo scaleId env _ = do
    divisions <- parseDivisions env
    intervals <- parseIntervals divisions env
    let layout = Theory.layout intervals
    let fmt = TheoryFormat.letters (Theory.layoutPcPerOctave layout)
    let defaultKey = Theory.key (Pitch.Degree 0 0) "" intervals layout
    let smap = (ChromaticScales.scaleMap layout fmt mempty defaultKey)
            { ChromaticScales.smapSemisToNn = \_config ->
                pure . semisToNn divisions
            }
    pure $ ChromaticScales.makeScale scaleId smap "unused doc"

semisToNn :: Int -> Pitch.FSemi -> Pitch.NoteNumber
semisToNn divisions = Pitch.NoteNumber . (+12) . (*step)
    where step = 12 / fromIntegral divisions

edoDivisions :: Env.Key
edoDivisions = "edo-divisions"

edoIntervals :: Env.Key
edoIntervals = "edo-intervals"

parseDivisions :: Env.Environ -> Either DeriveT.PitchError Int
parseDivisions =
    Scales.readEnviron (Just . Typecheck.positive) Nothing edoDivisions

parseIntervals :: Int -> Env.Environ -> Either DeriveT.PitchError [Pitch.Semi]
parseIntervals divisions =
    Scales.readEnviron_ (check <=< parse)
        (Just (Right (replicate divisions 1))) edoIntervals
    where
    parse (Left xs) = Right xs
    parse (Right sym) = maybe
        (Left $ Just $ "not one of: " <> pretty (Map.keys namedIntervals))
        Right (Map.lookup sym namedIntervals)
    check intervals
        | Num.sum intervals == divisions = Right intervals
        | otherwise = Left $ Just $ "sum " <> pretty intervals
            <> " should equal divisions " <> pretty divisions

namedIntervals :: Map Text [Pitch.Semi]
namedIntervals = Map.fromList Just.namedIntervals
