module Derive.Scale.Octa where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TwelveUtil as TwelveUtil
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales = [octa21, octa12]

-- * octa21

octa21 :: Scale.Scale
octa21 = Scale.Scale
    { Scale.scale_id = octa21_id
    , Scale.scale_pattern = "[-1-9][a-h](b|bb|#|x)?"
    , Scale.scale_map = TwelveUtil.scale_map system
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = TwelveUtil.transpose system
    , Scale.scale_enharmonics = TwelveUtil.enharmonics system
    , Scale.scale_note_to_call = TwelveUtil.note_to_call system
    , Scale.scale_input_to_note = TwelveUtil.input_to_note system
    , Scale.scale_input_to_nn = Util.direct_input_to_nn
    }
    where
    system = TwelveUtil.system layout all_pitches keys default_key
        where Just default_key = Map.lookup (Pitch.Key "a-21") keys
    keys = all_keys layout "21"
    layout = Theory.layout [2, 1, 2, 1, 2, 1, 2, 1]

octa21_id :: Pitch.ScaleId
octa21_id = Pitch.ScaleId "octa21"


-- * octa12

octa12 :: Scale.Scale
octa12 = Scale.Scale
    { Scale.scale_id = octa12_id
    , Scale.scale_pattern = "[-1-9][a-h](b|bb|#|x)?"
    , Scale.scale_map = TwelveUtil.scale_map system
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = TwelveUtil.transpose system
    , Scale.scale_enharmonics = TwelveUtil.enharmonics system
    , Scale.scale_note_to_call = TwelveUtil.note_to_call system
    , Scale.scale_input_to_note = TwelveUtil.input_to_note system
    , Scale.scale_input_to_nn = Util.direct_input_to_nn
    }
    where
    system = TwelveUtil.system layout all_pitches keys default_key
        where Just default_key = Map.lookup (Pitch.Key "a-12") keys
    keys = all_keys layout "12"
    layout = Theory.layout [1, 2, 1, 2, 1, 2, 1, 2]

octa12_id :: Pitch.ScaleId
octa12_id = Pitch.ScaleId "octa12"

all_notes :: [Theory.Note]
all_notes = [Theory.Note pc accs | pc <- [0..7], accs <- [-1..1]]

all_pitches :: [Theory.Pitch]
all_pitches = [Theory.Pitch oct note | oct <- [-2..9], note <- all_notes]

make_keys :: Theory.Layout -> String -> [Theory.Semi] -> [Theory.Key]
make_keys layout name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- all_notes, abs (Theory.note_accidentals tonic) <= 1]

all_keys :: Theory.Layout -> String -> Map.Map Pitch.Key Theory.Key
all_keys layout name = Map.fromList $ zip (map Theory.show_key keys) keys
    where
    keys = make_keys layout name $
        Vector.toList (Theory.layout_intervals layout)
