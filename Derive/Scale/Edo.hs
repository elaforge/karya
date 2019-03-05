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
make scale_id = Scale.Make scale_id (pattern, call_doc) (make_edo scale_id)
    where
    pattern = TheoryFormat.fmt_pattern $ TheoryFormat.letters 25
    call_doc = Scales.annotate_call_doc Scales.standard_transposers
        doc [] Scales.default_scale_degree_doc
    doc = "Construct a scale from equal divisions of the octave.\
        \ The number of divisions are given by " <> ShowVal.doc edo_divisions
        <> ". The intervals that correspond to diatonic notes are from "
        <> ShowVal.doc edo_intervals <> ", which defaults to all 1s.\
        \ Because the diatonic intervals are given explicitly, there are no\
        \ keys, and because the notes per octave is variable, diatonic notes\
        \ start from A, rather than the conventional C."

make_edo :: Pitch.ScaleId -> Env.Environ -> Scale.LookupScale
    -> Either DeriveT.PitchError Scale.Scale
make_edo scale_id env _ = do
    divisions <- parse_divisions env
    intervals <- parse_intervals divisions env
    let layout = Theory.layout intervals
    let fmt = TheoryFormat.letters (Theory.layout_pc_per_octave layout)
    let default_key = Theory.key (Pitch.Degree 0 0) "" intervals layout
    let smap = (ChromaticScales.scale_map layout fmt mempty default_key)
            { ChromaticScales.smap_semis_to_nn = \_config ->
                return . semis_to_nn divisions
            }
    return $ ChromaticScales.make_scale scale_id smap "unused doc"

semis_to_nn :: Int -> Pitch.FSemi -> Pitch.NoteNumber
semis_to_nn divisions = Pitch.NoteNumber . (+12) . (*step)
    where step = 12 / fromIntegral divisions

edo_divisions :: Env.Key
edo_divisions = "edo-divisions"

edo_intervals :: Env.Key
edo_intervals = "edo-intervals"

parse_divisions :: Env.Environ -> Either DeriveT.PitchError Int
parse_divisions =
    Scales.read_environ (Just . Typecheck.positive) Nothing edo_divisions

parse_intervals :: Int -> Env.Environ -> Either DeriveT.PitchError [Pitch.Semi]
parse_intervals divisions =
    Scales.read_environ_ (check <=< parse)
        (Just (Right (replicate divisions 1))) edo_intervals
    where
    parse (Left xs) = Right xs
    parse (Right sym) = maybe
        (Left $ Just $ "not one of: " <> pretty (Map.keys named_intervals))
        Right (Map.lookup sym named_intervals)
    check intervals
        | Num.sum intervals == divisions = Right intervals
        | otherwise = Left $ Just $ "sum " <> pretty intervals
            <> " should equal divisions " <> pretty divisions

named_intervals :: Map Text [Pitch.Semi]
named_intervals = Map.fromList Just.named_intervals
