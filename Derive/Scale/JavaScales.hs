-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JavaScales where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Debug as Debug
import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Vector

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
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch

import           Global
import           Util.Affine


data Key = Key {
    key_name :: Text
    , key_start :: Pitch.PitchClass
    , key_intervals :: Intervals
    } deriving (Eq, Show)

type Intervals = Vector.Vector Pitch.Semi

data ScaleMap = ScaleMap {
    smap_layout :: Layout
    , smap_laras_map :: Map Text BaliScales.Laras
    , smap_default_laras :: BaliScales.Laras
    -- TODO fmt
    }

smap_pc_per_octave :: ScaleMap -> Pitch.PitchClass
smap_pc_per_octave = pc_per_octave . layout_intervals . smap_layout

make_scale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = "pattern" -- TODO use fmt
    , scale_symbols = []
    , scale_transposers = Scales.standard_transposers
    , scale_read = \_env -> read_pitch smap
    , scale_show = \_env -> show_pitch smap
    -- TODO technically it can change per laras, but can I forbid that?
    , scale_bottom = BaliScales.laras_base (smap_default_laras smap)
    , scale_layout = layout_intervals (smap_layout smap)
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

data Layout = Layout {
    layout_start :: Int -- Chromatic, but usually added to PitchClass etc.
    , layout_intervals :: Intervals
    , layout_theory :: Theory.Layout -- TODO remove, only BaliScales.semis_to_nn
    , layout_degree_to_pc :: Map Pitch.Degree Pitch.PitchClass
    } deriving (Show)

make_layout :: Int -> [Pitch.Semi] -> Layout
make_layout start intervals = Layout
    { layout_start = start
    , layout_intervals = Vector.fromList intervals
    , layout_theory = Theory.layout intervals
    , layout_degree_to_pc = Map.fromList $ zip to_degree [0..]
    }
    where to_degree = make_step_to_degree intervals

make_step_to_degree :: [Pitch.Semi] -> [Pitch.Degree]
make_step_to_degree = concat . zipWith make [0..]
    where make pc acc = map (Pitch.Degree pc) [0 .. acc-1]

lima_intervals, barang_intervals :: Intervals
lima_intervals = Vector.fromList [1, 1, 2, 1, 2]
barang_intervals = Vector.fromList [1, 2, 1, 1, 2] -- 23567


-- .      1   2   3   4   5   6   7   1^
-- lima   00  10  20  21  30  40  41  00
-- barang 41  00  10  11  20  30  40  41
-- barang 01  10  20  21  30  40  00  01
-- Step   0   1   2   3   4   5   6   0
--        00  10  11  20  30  40  41  00

show_pitch :: ScaleMap -> Pitch.Pitch
    -> Either DeriveT.PitchError Pitch.Note
show_pitch smap (Pitch.Pitch oct (Pitch.Degree pc _))
    | 0 <= oct && oct <= 9 && 0 <= pc && pc < smap_pc_per_octave smap =
        Right $ Pitch.Note $ showt oct <> showt (pc + 1)
    | otherwise = Left DeriveT.InvalidInput

read_pitch :: ScaleMap -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
read_pitch smap note = do
    (oct, pc) <- fmap (subtract 1) <$> parse p_absolute_pitch note
    if 0 <= oct && oct <= 9 && 0 <= pc && pc < smap_pc_per_octave smap
        then Right $ Pitch.Pitch oct (Pitch.Degree pc 0)
        else Left $ DeriveT.InvalidInput

parse :: A.Parser a -> Pitch.Note -> Either DeriveT.PitchError a
parse p note = maybe (Left $ DeriveT.UnparseableNote note) Right $
    ParseText.maybe_parse p (Pitch.note_text note)

p_absolute_pitch :: A.Parser (Pitch.Octave, Pitch.PitchClass)
p_absolute_pitch = (,) <$> p_digit <*> p_digit

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
        chromatic_to_nn smap config $
        pitch_to_transposed layout pitch controls
    pitch_note :: Pitch.Pitch -> Scale.PitchNote
    pitch_note pitch (PSignal.PitchConfig _env controls) =
        show_pitch smap $ chromatic_to_pitch layout $
        round_chromatic $ pitch_to_transposed layout pitch controls
    layout = smap_layout smap

-- Adapt to ChromaticScales.SemisToNoteNumber
-- TODO which should use typed FChromatic instead of untyped Pitch.FSemi
chromatic_to_nn :: ScaleMap -> PSignal.PitchConfig -> FChromatic
    -> Either DeriveT.PitchError Pitch.NoteNumber
chromatic_to_nn smap config fc =
    BaliScales.semis_to_nn (layout_theory (smap_layout smap))
        (smap_laras_map smap) (smap_default_laras smap)
        config (to_semis fc)
    where
    -- Adapt to Pitch.FSemi taken by SemisToNoteNumber.
    -- pitch_to_chromatic subtracts layout_start, so I have to add it back
    -- Because FSemis are absolute, while Chromatic is scale-relative.
    to_semis (FChromatic fc) =
        fc + fromIntegral (layout_start (smap_layout smap))

{-
-- | Pitch plus transposition to absolute semis.
pitch_to_transposed :: Layout -> Pitch.Pitch -> ScoreT.ControlValMap
    -> Pitch.FSemi
pitch_to_transposed layout pitch controls =
    octave * fromIntegral (semis_per_octave intervals) + fromIntegral semis
        + chromatic + dsteps
    where
    FChromaticSteps dsteps = diatonic_to_chromatic_frac
        intervals (degree_chromatic layout pitch) diatonic
    semis = pitch_to_semis intervals pitch
    Chromatic c = pitch_to_chromatic layout pitch
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = FDiatonicSteps $ get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    intervals = Theory.layout_intervals $ layout_intervals layout

pitch_to_semis :: Intervals -> Pitch.Pitch -> Pitch.Semi
pitch_to_semis layout (Pitch.Pitch oct degree) =
    oct * semis_per_octave layout + Pitch.degree_pc degree

semis_to_pitch :: Intervals -> Pitch.Semi -> Pitch.Pitch
semis_to_pitch intervals semis = Pitch.Pitch oct (Pitch.Degree steps 0)
    where (oct, steps) = semis `divMod` semis_per_octave intervals

semis_per_octave :: Intervals -> Int
semis_per_octave = Vector.sum
-}

-- | Pitch plus transposition to absolute chromatic.
pitch_to_transposed :: Layout -> Pitch.Pitch -> ScoreT.ControlValMap
    -> FChromatic
pitch_to_transposed layout pitch controls =
    -- Debug.trace_ret "trans" (pitch, fc, diatonic, dsteps, csteps) $
    fc .+^ (octave * per_oct + dsteps + csteps)
    where
    fc = fchromatic $ pitch_to_chromatic layout pitch
    per_oct = fcsteps $ c_per_oct intervals
    dsteps = diatonic_to_chromatic_frac
        intervals (degree_chromatic layout pitch) diatonic
    octave = FChromaticSteps $ get Controls.octave
    csteps = FChromaticSteps $ get Controls.chromatic
    diatonic = FDiatonicSteps $ get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    intervals = layout_intervals layout

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap _env (Pitch.Input kbd_type pitch _frac) = do
    -- Tonic is only to reverse to_absolute, but JavaScales doesn't use that
    -- notion of relative, instead expand_pitch adds steps after being
    -- flattened into the non-layout format.
    let tonic = 0
    pitch <- Scales.kbd_to_scale kbd_type pc_per_octave tonic pitch
    show_pitch smap =<< expand_pitch (smap_layout smap) pitch
    where
    DiatonicSteps pc_per_octave =
        d_per_oct (layout_intervals (smap_layout smap))

-- | Input Pitches come in with a layout, to match the notion of diatonic
-- steps, but internally Pitch is absolute with only PitchClass.
--
-- Convert Pitches where pitch_pc is Diatonic and pitch_accidentals is
-- Chromatic, to Pitch where pitch_pc is Chromatic.
expand_pitch :: Layout -> Pitch.Pitch
    -> Either PSignal.PitchError Pitch.Pitch
expand_pitch layout (Pitch.Pitch oct degree) =
    case Map.lookup degree (layout_degree_to_pc layout) of
        Nothing -> Left DeriveT.InvalidInput
        Just step -> Right $
            Pitch.add_pc per_oct (layout_start layout) $
            Pitch.Pitch oct (Pitch.Degree step 0)
    where
    ChromaticSteps per_oct = c_per_oct (layout_intervals layout)


-- ** transpose

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition _env steps pitch = Right $ case transposition of
    Scale.Diatonic -> transpose_diatonic layout pitch (DiatonicSteps steps)
    Scale.Chromatic -> add_pitch intervals (ChromaticSteps steps) pitch
    where
    intervals = layout_intervals $ smap_layout smap
    layout = smap_layout smap

transpose_diatonic :: Layout -> Pitch.Pitch -> DiatonicSteps -> Pitch.Pitch
transpose_diatonic layout pitch steps = add_pitch intervals csteps pitch
    where
    csteps = -- Debug.trace_ret "steps" (add_diatonic intervals c steps, c) $
        add_diatonic intervals c steps .-. c
    c = pitch_to_chromatic layout pitch
    intervals = layout_intervals layout

-- TODO Without octave, should I include it?
-- I think it works for transpose because I subtract off the absolute Chromatic
-- again anyway?
degree_chromatic :: Layout -> Pitch.Pitch -> Chromatic
degree_chromatic layout =
    Chromatic . subtract (layout_start layout) . Pitch.pitch_pc
    -- Chromatic and Diatonic are supposedly absolute, but can't be.
    -- The layout is relative to Diatonic 0, because it has to be, because
    -- in pelog barang absolute 0 (aka "1") is not a diatonic pitch.
    -- So Diatonic has to be relative to layout_start, which means Chromatic
    -- also should be, because otherwise I'd have to push layout_start
    -- down to to_chromatic and it's easier to do it up here.  Also nicer if
    -- both Chromatic and Diatonic are relative to the same thing.

add_pitch :: Intervals -> ChromaticSteps -> Pitch.Pitch -> Pitch.Pitch
add_pitch intervals (ChromaticSteps steps) = Pitch.add_pc per_oct steps
    where ChromaticSteps per_oct = c_per_oct intervals

pitch_to_chromatic :: Layout -> Pitch.Pitch -> Chromatic
pitch_to_chromatic layout (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Chromatic $ oct * per_oct + pc - layout_start layout
    where
    ChromaticSteps per_oct = c_per_oct (layout_intervals layout)

chromatic_to_pitch :: Layout -> Chromatic -> Pitch.Pitch
chromatic_to_pitch layout (Chromatic c) = Pitch.Pitch oct (Pitch.Degree pc 0)
    where
    (oct, pc) = (c + layout_start layout) `divMod` per_oct
    ChromaticSteps per_oct = c_per_oct (layout_intervals layout)


-- * typed implementation

-- | Convert a fractional number of diatonic steps to chromatic steps, starting
-- from the given chromatic pitch.
--
-- Chromatic is not absolute in that it doesn't include octave, but it is 1-7.
-- I could make absolute easily, or just add octaves back on later.
diatonic_to_chromatic_frac :: Intervals -> Chromatic -> FDiatonicSteps
    -> FChromaticSteps
diatonic_to_chromatic_frac intervals start steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1))
        (FChromaticSteps frac)
    | otherwise = Num.scale (transpose (isteps-1)) (transpose isteps)
        (FChromaticSteps (1 - abs frac))
    where
    (isteps, frac) = split_diatonic steps
    transpose steps = fcsteps $ add_diatonic intervals start steps .-. start


-- | Convert diatonic steps to chromatic steps, starting from the given
-- chromatic pitch.
--
-- Not every Chromatic corresponds to a Diatonic.  So it's unclear how to
-- diatonically transpose a note that's not diatonic in the first place.  I've
-- defined that the first diatonic step will take the note to the next
-- Diatonic.
add_diatonic :: Intervals -> Chromatic -> DiatonicSteps -> Chromatic
add_diatonic intervals start steps
    | steps == 0 = start
    | otherwise = to_chromatic intervals (d .+^ steps2)
    where
    (d, cs) = to_diatonic intervals start
    -- cs is the remainder for a non-diatonic start.  If >0, d has been rounded
    -- down.  So when going down, the first diatonic step is already accounted
    -- for in the round down.
    steps2 = if steps < 0 && cs > 0 then steps + 1 else steps

-- | Cannot convert Chromatic to Diatonic because not every chromatic step is
-- in a scale.  So I need a leftover.
to_diatonic :: Intervals -> Chromatic -> (Diatonic, ChromaticSteps)
to_diatonic intervals (Chromatic c) =
    (d .+^ DiatonicSteps oct * d_per_oct intervals, cs)
    where
    (d, cs) = c2d Vector.! c2
    (oct, c2) = c `divMod` fromIntegral (c_per_oct intervals)
    c2d = c_to_d intervals

-- 23567
-- d0 -> c1, because barang starts on 1
-- or, I could do the adjustment from Pitch: d0 -> c0 -> pc 1
to_chromatic :: Intervals -> Diatonic -> Chromatic
to_chromatic intervals (Diatonic d) =
    d2c Vector.! d2 .+^ ChromaticSteps oct * c_per_oct intervals
    where
    (oct, d2) = d `divMod` fromIntegral (d_per_oct intervals)
    d2c = d_to_c intervals

c_per_oct :: Intervals -> ChromaticSteps
c_per_oct = ChromaticSteps . Vector.sum

pc_per_octave :: Intervals -> Pitch.PitchClass
pc_per_octave = Vector.sum

d_per_oct :: Intervals -> DiatonicSteps
d_per_oct = DiatonicSteps . Vector.length

-- cd = c_to_d lima_intervals
-- dc = d_to_c lima_intervals
-- d2c = [0, 1, 2, 4, 5, 7]
--         1       2       3       4      5  6   7
-- c2d = [(0, 0), (1, 0), (2, 0), (2, 1), 3, 4, (4, 1)]

-- TODO cache these in Layout
d_to_c :: Intervals -> Vector.Vector Chromatic
d_to_c = Vector.map Chromatic . Vector.scanl (+) 0

c_to_d :: Intervals -> Vector.Vector (Diatonic, ChromaticSteps)
c_to_d intervals = Vector.fromList
    [ (Diatonic pc, ChromaticSteps acc)
    | (pc, steps) <- zip [0..] (Vector.toList intervals)
    , acc <- Lists.range' 0 steps 1
    ]

-- * conversions

round_chromatic :: FChromatic -> Chromatic
round_chromatic (FChromatic fc) = Chromatic (round fc)

fchromatic :: Chromatic -> FChromatic
fchromatic (Chromatic a) = FChromatic (fromIntegral a)

fcsteps :: ChromaticSteps -> FChromaticSteps
fcsteps (ChromaticSteps a) = FChromaticSteps (fromIntegral a)

fdiatonic :: DiatonicSteps -> FDiatonicSteps
fdiatonic (DiatonicSteps a) = FDiatonicSteps (fromIntegral a)

split_diatonic :: FDiatonicSteps -> (DiatonicSteps, Double)
split_diatonic (FDiatonicSteps d) = first DiatonicSteps (properFraction d)

{-
transpose_diatonic :: Intervals -> Pitch.Pitch -> Int -> Pitch.Pitch
transpose_diatonic intervals pitch steps =
    Pitch.add_pc (semis_per_octave intervals) semis pitch
    where
    semis = Debug.trace_retp "tr" (pitch, steps) $
        chromatic_steps intervals (Pitch.pitch_pc pitch) steps

-- | Convert diatonic steps to chromatic steps.
-- convert pc -> dia, dia + steps, new dia -> pc
chromatic_steps :: Intervals -> Int -> Int -> Int
chromatic_steps intervals pc steps =
    to_pc intervals (to_diatonic intervals (steps >= 0) pc + steps) - pc

-- | Diatonic steps to chromatic.
to_pc :: Intervals -> Int -> Pitch.PitchClass
to_pc intervals diatonic =
    oct * semis_per_octave intervals + Vector.sum (Vector.take steps intervals)
    where
    (oct, steps) = diatonic `divMod` diatonic_per_octave intervals
    diatonic_per_octave :: Intervals -> Int
    diatonic_per_octave = Vector.length

-- Problem is add diatonic to non-diatonic note.  It should step to next
-- diatonic.  But that means +1 when transpoing up, -1 when down.
-- It's like Diatonic is fractional, round up or down.
to_diatonic :: Intervals -> Bool -> Pitch.PitchClass -> Int
to_diatonic intervals _round_up pc = Util.Vector.find_before pc intervals

-- | Convert a fractional number of diatonic steps to chromatic steps.
diatonic_to_chromatic :: Intervals -> Pitch.PitchClass -> Double -> Pitch.FSemi
diatonic_to_chromatic intervals pc steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1)) frac
    | otherwise =
        Num.scale (transpose (isteps-1)) (transpose isteps) (1 - abs frac)
    where
    (isteps, frac) = properFraction steps
    transpose = fromIntegral . chromatic_steps intervals pc
-}


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
