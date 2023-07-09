-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JavaScales where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Debug as Debug
import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch

import           Global


data Key = Key {
    key_name :: Text
    , key_start :: Pitch.PitchClass
    , key_intervals :: Intervals
    } deriving (Eq, Show)

type Intervals = Vector.Vector Pitch.Semi

data ScaleMap = ScaleMap {
    smap_layout :: Layout
    , smap_start :: Step
    , smap_laras_map :: Map Text BaliScales.Laras
    , smap_default_laras :: BaliScales.Laras
    -- TODO fmt
    }

make_scale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = "pattern" -- TODO
    , scale_symbols = []
    , scale_transposers = Scales.standard_transposers
    , scale_read = \_env -> read_pitch smap
    , scale_show = \_env -> show_pitch smap
    -- TODO technically it can change per laras, but can I forbid that?
    , scale_bottom = BaliScales.laras_base (smap_default_laras smap)
    , scale_layout = Theory.layout_intervals
        (layout_intervals (smap_layout smap))
    , scale_transpose = transpose smap
    , scale_enharmonics = Scales.no_enharmonics
    , scale_note_to_call = note_to_call scale smap
    , scale_input_to_note = input_to_note smap
    , scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note smap) (note_to_call scale smap)
    -- TODO
    , scale_call_doc = Scales.scale_degree_doc ScaleDegree.scale_degree
    }
    where scale = PSignal.Scale scale_id Scales.standard_transposers

-- | Like PitchClass, except independent of layout and with no accidentals,
-- e.g. 0-6.
type Step = Int

data Layout = Layout {
    layout_intervals :: Theory.Layout -- TODO should be Theory.Intervals
    , layout_step_to_degree :: Vector.Vector Pitch.Degree
    , layout_degree_to_step :: Map Pitch.Degree Step
    } deriving (Show)

make_layout :: [Pitch.Semi] -> Layout
make_layout intervals = Layout
    { layout_intervals = Theory.layout intervals
    , layout_step_to_degree = Vector.fromList to_degree
    , layout_degree_to_step = Map.fromList $ zip to_degree [1..]
    }
    where to_degree = make_step_to_degree intervals

make_step_to_degree :: [Pitch.Semi] -> [Pitch.Degree]
make_step_to_degree = concat . zipWith make [0..]
    where make pc acc = map (Pitch.Degree pc) [0 .. acc-1]

lima_layout = Theory.layout [1, 1, 2, 1, 2]
barang_layout = Theory.layout [1, 2, 1, 1, 2] -- 23567
    -- , ("barang", 1, [1, 2, 1, 1, 2]) -- 23567

-- t0 = make_step_to_degree [1, 1, 2, 1, 2]
-- t1 = make_step_to_degree [1, 2, 1, 1, 2]

-- .      1   2   3   4   5   6   7   1^
-- lima   00  10  20  21  30  40  41  00
-- barang 41  00  10  11  20  30  40  41
-- barang 01  10  20  21  30  40  00  01
-- Step   0   1   2   3   4   5   6   0
--        00  10  11  20  30  40  41  00

show_pitch :: ScaleMap -> Pitch.Pitch
    -> Either DeriveT.PitchError Pitch.Note
show_pitch smap (Pitch.Pitch oct degree) =
    case Map.lookup degree (layout_degree_to_step (smap_layout smap)) of
        Nothing -> Left DeriveT.InvalidInput
        Just step -> return $ Pitch.Note $ showt oct <> showt step

read_pitch :: ScaleMap -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
read_pitch smap note = do
    (oct, step) <- parse p_absolute_pitch note
    case layout_step_to_degree (smap_layout smap) Vector.!? step of
        Nothing -> Left $ DeriveT.UnparseableNote note
        Just degree -> return $ Pitch.Pitch oct degree

parse :: A.Parser a -> Pitch.Note -> Either DeriveT.PitchError a
parse p note = maybe (Left $ DeriveT.UnparseableNote note) Right $
    ParseText.maybe_parse p (Pitch.note_text note)

p_absolute_pitch :: A.Parser (Pitch.Octave, Step)
p_absolute_pitch = (,) <$> p_digit <*> (subtract 1 <$> p_digit)

p_digit :: A.Parser Int
p_digit = maybe mzero pure . Num.readDigit =<< A.satisfy ParseText.is_digit

note_to_call :: DeriveT.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale smap note = case read_pitch smap note of
    Left _ -> Nothing
    Right pitch -> Just $ ScaleDegree.scale_degree scale
        (pitch_nn pitch) (pitch_note pitch)
    where
    -- | Create a PitchNn for 'ScaleDegree.scale_degree'.
    pitch_nn :: Pitch.Pitch -> Scale.PitchNn
    pitch_nn pitch config@(PSignal.PitchConfig _env controls) =
        semis_to_nn smap config $
            pitch_to_transposed intervals pitch controls
    pitch_note :: Pitch.Pitch -> Scale.PitchNote
    pitch_note pitch (PSignal.PitchConfig _env controls) =
        show_pitch smap $ semis_to_pitch (smap_layout smap) $ round $
            pitch_to_transposed intervals pitch controls
    intervals = layout_intervals $ smap_layout smap

semis_to_nn :: ScaleMap -> ChromaticScales.SemisToNoteNumber
semis_to_nn smap =
    BaliScales.semis_to_nn (layout_intervals (smap_layout smap))
        (smap_laras_map smap) (smap_default_laras smap)

-- TODO use Theory.Intervals instead of Layout
pitch_to_transposed :: Theory.Layout -> Pitch.Pitch -> ScoreT.ControlValMap
    -> Pitch.FSemi
pitch_to_transposed intervals pitch controls =
    octave * fromIntegral semis_per_octave + fromIntegral semis
        + chromatic + dsteps
    where
    dsteps = Theory.simple_diatonic_to_chromatic
        (Theory.layout_intervals intervals)
        (Pitch.pitch_pc pitch) diatonic
    semis = Theory.pitch_to_semis intervals pitch
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    semis_per_octave = Theory.layout_semis_per_octave intervals
    get m = Map.findWithDefault 0 m controls

semis_to_pitch :: Layout -> Pitch.Semi -> Pitch.Pitch
semis_to_pitch layout semis =
    Pitch.Pitch oct $ layout_step_to_degree layout Vector.! steps
    where
    (oct, steps) = semis
        `divMod` Theory.layout_semis_per_octave (layout_intervals layout)
    -- Index ! should not fail because layout_semis_per_octave should be
    -- consistent with step_to_degree length.
    -- TODO check it though

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap _env (Pitch.Input kbd_type pitch _frac) = do
    -- let tonic = smap_start smap
    let tonic = 0
    -- tonic is only to reverse to_absolute
    pitch <- Debug.trace_retp "pitch" pitch <$>
        Scales.kbd_to_scale kbd_type pc_per_octave tonic $
        -- add_semis (smap_layout smap) (smap_start smap) pitch
        -- pitch
        Pitch.add_pc 5 (smap_start smap) pitch
    Debug.tracep "out" $ show_pitch smap pitch
    where
    pc_per_octave = Theory.layout_pc_per_octave $ layout_intervals $
        smap_layout smap

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition _env steps pitch = Right $ case transposition of
    -- Scale.Chromatic -> Right $ semis_to_pitch layout $ steps + semis
    --     where semis = Theory.pitch_to_semis intervals pitch
    Scale.Chromatic -> add_semis layout steps pitch
    Scale.Diatonic -> semis_to_pitch layout $ steps + floor dsteps
        where
        dsteps = Theory.simple_diatonic_to_chromatic
            (Theory.layout_intervals intervals)
            (Pitch.pitch_pc pitch) (fromIntegral steps)
    where
    layout = smap_layout smap
    intervals = layout_intervals layout

add_semis :: Layout -> Step -> Pitch.Pitch -> Pitch.Pitch
add_semis layout steps pitch =
    semis_to_pitch layout $ steps + Theory.pitch_to_semis intervals pitch
    where intervals = layout_intervals layout

add_steps :: Layout -> Step -> Pitch.Pitch -> Pitch.Pitch
add_steps = undefined

--     Scale.Diatonic -> Right $ semis_to_pitch layout $ steps + floor dsteps

-- ***


{-
pelog_format_abs :: TheoryFormat.Format
pelog_format_abs = TheoryFormat.Format
    -- For java, ignore key, no accidentals, just show
    -- { fmt_show = undefined -- show_pitch_absolute config degrees
    { fmt_show = undefined -- TheoryFormat.show_pitch_cipher config degrees
    , fmt_read = undefined -- p_pitch config degrees
    , fmt_to_absolute = undefined -- \_ -> Right . relative_to_absolute
    , fmt_pattern = undefined -- octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = undefined -- Vector.length degrees
    , fmt_relative = False
    }
    where
    degrees = cipher7
    config = undefined

-- Format doesn't do transposition!
pelog_format :: Map Pitch.Key Key -> Key -> TheoryFormat.Format
pelog_format keys default_key =
    TheoryFormat.make_relative_format "pattern" cipher7 fmt
    where
    fmt = TheoryFormat.RelativeFormat
        { rel_config = TheoryFormat.Config
            { config_show_octave = TheoryFormat.show_octave
            , config_parse_octave = TheoryFormat.parse_octave1
            , config_accidental = TheoryFormat.ascii_accidentals -- TODO Nothing
            }
        , rel_key_config = TheoryFormat.KeyConfig
            { key_parse = Scales.get_key default_key keys
            , key_default = default_key
            }
        , rel_show_degree = show_degree
        , rel_to_absolute = \_ _ -> TheoryFormat.relative_to_absolute
        }

-- type ShowDegree key = key -> ShowOctave -> Degrees -> AccidentalFormat
--     -> Either Pitch.Degree Pitch.Pitch -> Pitch.Note
show_degree :: TheoryFormat.ShowDegree Key
show_degree = undefined
-- show_degree key show_octave degrees acc_fmt degree_pitch =
--     Pitch.Note $ case degree_pitch of
--         Right (Pitch.Pitch oct degree) -> undefined
cipher7 :: TheoryFormat.Degrees
cipher7 = TheoryFormat.make_degrees (map showt [1..7])

pelog_absolute :: TheoryFormat.Format
pelog_absolute =
    TheoryFormat.make_absolute_format_config config "[0-9][1-7]" cipher7
    where
    config = TheoryFormat.default_config
        { TheoryFormat.config_parse_octave = TheoryFormat.parse_octave1 }

pelog_keys :: Map Pitch.Key Key
pelog_keys = Map.fromList $ map make
    --      4   7
    -- 1 2 3 5 6
    [ ("lima", 0, [1, 1, 2, 1, 2]) -- 12356
    , ("nem", 0, [1, 1, 2, 1, 2]) -- same as lima
    -- So it's relative keyboard, but absolute notation?
    --    4     1
    -- 2 3 5 6 7
    , ("barang", 1, [1, 2, 1, 1, 2]) -- 23567
    ]
    where
    make (name, start, intervals) =
        (Pitch.Key name, Key name start (Vector.fromList intervals))

slendro_keys :: Map Pitch.Key Key
slendro_keys = Map.fromList $ map (first Pitch.Key)
    [ ("nem", undefined) -- no 1?
    , ("sanga", undefined) -- no 3
    , ("manyura", undefined) -- no 5
    ]
-}
