-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Derive.Scale.JavaScales (
    ScaleMap(..)
    , Instrument(..)
    , Absolute(..)
    , make_scale
    , Layout(..)
    , make_layout
    -- * Format
    , Format(..)
    , cipher_absolute
    , cipher_octave_relative
#ifdef TESTING
    , add_diatonic
    , module Derive.Scale.JavaScales
#endif
) where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch

import           Global
import           Util.Affine


data ScaleMap = ScaleMap {
    layout :: Layout
    , laras_map :: Map Text BaliScales.Laras
    , default_laras :: BaliScales.Laras
    , format :: Format
    }

data Instrument = Instrument {
    center :: Pitch.Octave
    , bottom :: Absolute
    -- | Unlike most ranges, this is inclusive, so top is a valid pitch.
    -- This seems more intuitive for instruments, and besides there's no
    -- need for an empty range.
    , top :: Absolute
    } deriving (Show)

check_range :: Instrument -> Absolute -> Either DeriveT.PitchError Absolute
check_range inst pitch
    | inst.bottom <= pitch && pitch <= inst.top = Right pitch
    | otherwise = Left $ DeriveT.PitchError $
        "out of instrument range " <> pretty inst.bottom <> " to "
        <> pretty inst.top <> ": " <> pretty pitch

-- | An absolute pitch as parsed from Pitch.Note, so e.g. 1-7 (for 1-7).  This
-- is the "chromatic" representation, while Pitch is the diatonic one, taking
-- pathet into account.
--
-- TheoryFormat has a similar notion of relative to absolute, but it's
-- the other way around, in that Pitch.Pitch is the absolute one, while
-- Pitch.Note is relative.
data Absolute = Absolute !Pitch.Octave !Pitch.PitchClass
    deriving (Show, Eq, Ord)

instance Pretty Absolute where
    pretty (Absolute oct pc) = showt oct <> showt pc

make_scale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = smap.format.pattern
    , scale_symbols = []
    , scale_transposers = Scales.standard_transposers
    , scale_read = \_env -> smap.format.read
    , scale_show = \_env -> smap.format.show
    -- TODO technically it can change per laras, but can I forbid that?
    , scale_bottom = BaliScales.laras_base smap.default_laras
    , scale_layout = smap.layout.intervals
    , scale_transpose = transpose smap
    , scale_enharmonics = Scales.no_enharmonics
    , scale_note_to_call = note_to_call scale smap
    , scale_input_to_note = input_to_note smap
    , scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note smap) (note_to_call scale smap)
    -- TODO
    , scale_call_doc = Scales.scale_degree_doc ScaleDegree.scale_degree
    }
    where
    scale = PSignal.Scale scale_id Scales.standard_transposers

-- TODO rename to Key or Pathet?
data Layout = Layout {
    -- | 'Absolute' (chromatic) step on which 'intervals' starts, to
    -- map from Absolute to Pitch and back.
    start :: Pitch.PitchClass
    , intervals :: Intervals
    , theory :: Theory.Layout -- TODO remove, only BaliScales.semis_to_nn
    -- | Cache diatonic to chromatic mappings.
    , d_to_c :: Vector.Vector Chromatic
    , c_to_d :: Vector.Vector (Diatonic, ChromaticSteps)
    -- | This is like d_to_c, except it includes chromatic steps as
    -- accidentals, so it can map from the ascii kbd.
    -- TODO merge with d_to_c.
    , degree_to_pc :: Map Pitch.Degree Pitch.PitchClass
    } deriving (Show)

type Intervals = Vector.Vector Pitch.Semi

make_layout :: Int -> [Pitch.Semi] -> Layout
make_layout start intervals = Layout
    { start = start
    , intervals = intervals_v
    , theory = Theory.layout intervals
    , d_to_c = make_d_to_c intervals_v
    , c_to_d = make_c_to_d intervals_v
    , degree_to_pc = Map.fromList $ zip to_degree [0..]
    }
    where
    to_degree = make_step_to_degree intervals
    intervals_v = Vector.fromList intervals

make_step_to_degree :: [Pitch.Semi] -> [Pitch.Degree]
make_step_to_degree = concat . zipWith make [0..]
    where make pc acc = map (Pitch.Degree pc) [0 .. acc-1]

lima = make_layout 0 [1, 1, 2, 1, 2]
barang = make_layout 1 [1, 2, 1, 1, 2] -- 23567

note_to_call :: DeriveT.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale smap note = case smap.format.read note of
    Left _ -> Nothing
    Right pitch -> Just $ ScaleDegree.scale_degree scale
        (pitch_nn pitch) (pitch_note pitch)
    where
    pitch_nn :: Pitch.Pitch -> Scale.PitchNn
    pitch_nn pitch config@(PSignal.PitchConfig _env controls) =
        chromatic_to_nn smap config $
        pitch_to_transposed layout pitch controls
    pitch_note :: Pitch.Pitch -> Scale.PitchNote
    pitch_note pitch (PSignal.PitchConfig _env controls) =
        smap.format.show $ chromatic_to_pitch layout $
        round_chromatic $ pitch_to_transposed layout pitch controls
    layout = smap.layout

-- Adapt to ChromaticScales.SemisToNoteNumber
-- TODO which should use typed FChromatic instead of untyped Pitch.FSemi
chromatic_to_nn :: ScaleMap -> PSignal.PitchConfig -> FChromatic
    -> Either DeriveT.PitchError Pitch.NoteNumber
chromatic_to_nn smap config fc =
    BaliScales.semis_to_nn smap.layout.theory smap.laras_map smap.default_laras
        config (to_semis fc)
    where
    -- Adapt to Pitch.FSemi taken by SemisToNoteNumber.  Pitch.Pitch is
    -- relative to layout.start, so I have to add it back Because FSemis are
    -- absolute, while Chromatic is scale-relative.
    to_semis (FChromatic fc) = fc + fromIntegral smap.layout.start

-- | Pitch plus transposition to absolute chromatic.
pitch_to_transposed :: Layout -> Pitch.Pitch -> ScoreT.ControlValMap
    -> FChromatic
pitch_to_transposed layout pitch controls =
    fc .+^ (octave * per_oct + dsteps + csteps)
    where
    fc = fchromatic $ pitch_to_chromatic layout pitch
    per_oct = fromIntegral $ c_per_oct layout.intervals
    dsteps = diatonic_to_chromatic_frac
        layout (pitch_to_chromatic layout pitch) diatonic
    octave = FChromaticSteps $ get Controls.octave
    csteps = FChromaticSteps $ get Controls.chromatic
    diatonic = FDiatonicSteps $ get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap _env (Pitch.Input kbd_type pitch _frac) = do
    -- Tonic is to reverse to_absolute, but JavaScales are the opposite way,
    -- displayed absolute, see 'Absolute'.
    let tonic = 0
    pitch <- Scales.kbd_to_scale kbd_type per_octave tonic pitch
    smap.format.show pitch
    where per_octave = d_per_oct smap.layout.intervals


-- ** transpose

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition _env steps pitch = case transposition of
    Scale.Diatonic ->
        Right $ transpose_diatonic smap.layout pitch (DiatonicSteps steps)
    Scale.Chromatic ->
        Right $ transpose_chromatic smap.layout pitch (ChromaticSteps steps)

transpose_diatonic :: Layout -> Pitch.Pitch -> DiatonicSteps -> Pitch.Pitch
transpose_diatonic layout pitch steps =
    chromatic_to_pitch layout $
        add_diatonic layout (pitch_to_chromatic layout pitch) steps

transpose_chromatic :: Layout -> Pitch.Pitch -> ChromaticSteps -> Pitch.Pitch
transpose_chromatic layout pitch steps =
    chromatic_to_pitch layout (pitch_to_chromatic layout pitch .+^ steps)

pitch_to_chromatic :: Layout -> Pitch.Pitch -> Chromatic
pitch_to_chromatic layout (Pitch.Pitch oct (Pitch.Degree pc acc)) =
    layout.d_to_c Vector.! d
        .+^ ChromaticSteps (acc + (oct+oct2) * c_per_oct layout.intervals)
    where (oct2, d) = pc `divMod` d_per_oct layout.intervals

chromatic_to_pitch :: Layout -> Chromatic -> Pitch.Pitch
chromatic_to_pitch layout c = Pitch.Pitch oct (Pitch.Degree pc acc)
    where
    (Diatonic d, ChromaticSteps acc) = to_diatonic layout c
    (oct, pc) = d `divMod` d_per_oct layout.intervals

-- | Convert a fractional number of diatonic steps to chromatic steps, starting
-- from the given chromatic pitch.
--
-- Chromatic is not absolute in that it doesn't include octave, but it is 1-7.
-- I could make absolute easily, or just add octaves back on later.
diatonic_to_chromatic_frac :: Layout -> Chromatic -> FDiatonicSteps
    -> FChromaticSteps
diatonic_to_chromatic_frac layout start steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1))
        (FChromaticSteps frac)
    | otherwise = Num.scale (transpose (isteps-1)) (transpose isteps)
        (FChromaticSteps (1 - abs frac))
    where
    (isteps, frac) = split_diatonic steps
    transpose steps = fcsteps $ add_diatonic layout start steps .-. start

-- | Convert diatonic steps to chromatic steps, starting from the given
-- chromatic pitch.
--
-- Not every Chromatic corresponds to a Diatonic.  So it's unclear how to
-- diatonically transpose a note that's not diatonic in the first place.  I've
-- defined that the first diatonic step will take the note to the next
-- Diatonic.
add_diatonic :: Layout -> Chromatic -> DiatonicSteps -> Chromatic
add_diatonic layout start steps
    | steps == 0 = start
    | otherwise = to_chromatic layout (d .+^ steps2)
    where
    (d, cs) = to_diatonic layout start
    -- cs is the remainder for a non-diatonic start.  If >0, d has been rounded
    -- down.  So when going down, the first diatonic step is already accounted
    -- for in the round down.
    steps2 = if steps < 0 && cs > 0 then steps + 1 else steps

-- | Cannot convert Chromatic to Diatonic because not every chromatic step is
-- in a scale.  So I need a leftover.
to_diatonic :: Layout -> Chromatic -> (Diatonic, ChromaticSteps)
to_diatonic layout (Chromatic c) =
    (d .+^ DiatonicSteps (oct * d_per_oct layout.intervals), cs)
    where
    (d, cs) = layout.c_to_d Vector.! c2
    (oct, c2) = c `divMod` c_per_oct layout.intervals

to_chromatic :: Layout -> Diatonic -> Chromatic
to_chromatic layout (Diatonic d) =
    layout.d_to_c Vector.! d2 .+^ ChromaticSteps (oct * c_per_oct intervals)
    where
    (oct, d2) = d `divMod` d_per_oct intervals
    intervals = layout.intervals

-- | This should return DiatonicSteps, but only one caller wants that.
d_per_oct :: Intervals -> Int
d_per_oct = Vector.length

-- | This should return ChromaticSteps, but only one caller wants that.
c_per_oct :: Intervals -> Int
c_per_oct = Vector.sum

make_d_to_c :: Intervals -> Vector.Vector Chromatic
make_d_to_c = Vector.map Chromatic . Vector.scanl (+) 0

make_c_to_d :: Intervals -> Vector.Vector (Diatonic, ChromaticSteps)
make_c_to_d intervals = Vector.fromList
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


-- * Format

data Format = Format {
    show :: Pitch.Pitch -> Either DeriveT.PitchError Pitch.Note
    , read :: Pitch.Note -> Either DeriveT.PitchError Pitch.Pitch
    , pattern :: Text
    }

cipher_absolute :: Layout -> Format
cipher_absolute layout = Format
    { show = show_absolute layout
    , read = read_absolute layout
    , pattern = "[0-9][1-" <> showt (c_per_oct layout.intervals) <> "]"
    }

cipher_octave_relative :: Layout -> Instrument -> Format
cipher_octave_relative layout inst = Format
    { show = show_dotted_cipher layout inst
    , read = read_dotted_cipher layout inst.center
    , pattern = degree <> "|`" <> degree <> "[.^]+`"
    }
    where
    degree = "[1-" <> showt pc_per_octave <> "]"
    pc_per_octave = c_per_oct layout.intervals

show_dotted_cipher :: Layout -> Instrument -> Pitch.Pitch
    -> Either PSignal.PitchError Pitch.Note
show_dotted_cipher layout inst pitch = do
    Absolute oct pc <- check_range inst
        =<< maybe (Left DeriveT.InvalidInput) Right (to_absolute layout pitch)
    let delta = oct - inst.center
    let degree = showt pc
    return $ Pitch.Note $ if delta == 0 then degree else mconcat
        [ "`", degree
        , Text.replicate (abs delta) (if delta > 0 then "^" else ".")
        , "`"
        ]

read_dotted_cipher :: Layout -> Pitch.Octave -> Pitch.Note
    -> Either PSignal.PitchError Pitch.Pitch
read_dotted_cipher layout center =
    parse $ (mkpitch 0 =<< parse_pc) <|> p_with_octave
    where
    p_with_octave = do
        A.char '`'
        pc <- parse_pc
        octs <- A.many' $ A.satisfy $ \c -> c == '.' || c == '^'
        A.char '`'
        let oct = Lists.count (=='^') octs - Lists.count (=='.') octs
        mkpitch oct pc
    mkpitch oct pc = return $ from_absolute layout (Absolute (oct+center) pc)
    parse_pc = p_pc (c_per_oct layout.intervals)

show_absolute :: Layout -> Pitch.Pitch -> Either PSignal.PitchError Pitch.Note
show_absolute layout pitch = do
    -- show_pitch is used by input_to_note, InvalidInput is correct for that at
    -- least.
    Absolute oct pc <- maybe (Left DeriveT.InvalidInput) Right $
        to_absolute layout pitch
    return $ Pitch.Note $ showt oct <> showt pc

read_absolute :: Layout -> Pitch.Note -> Either PSignal.PitchError Pitch.Pitch
read_absolute layout note = do
    absolute <- parse
        (Absolute <$> p_octave <*> p_pc (c_per_oct layout.intervals)) note
    return $ from_absolute layout absolute

from_absolute :: Layout -> Absolute -> Pitch.Pitch
from_absolute layout (Absolute oct pc) =
    Pitch.Pitch (oct+oct2) (Pitch.Degree d cs)
    where
    (oct2, pc2) = (pc - 1 - layout.start) `divMod` c_per_oct layout.intervals
    (Diatonic d, ChromaticSteps cs) = layout.c_to_d Vector.! pc2

to_absolute :: Layout -> Pitch.Pitch -> Maybe Absolute
to_absolute layout (Pitch.Pitch oct degree) =
    case Map.lookup degree layout.degree_to_pc of
        Nothing -> Nothing
        Just pc -> Just $ Absolute (oct+oct2) (pc2+1)
            where
            (oct2, pc2) = (pc + layout.start)
                `divMod` c_per_oct layout.intervals

parse :: A.Parser a -> Pitch.Note -> Either DeriveT.PitchError a
parse p note = maybe (Left $ DeriveT.UnparseableNote note) Right $
    ParseText.maybe_parse p (Pitch.note_text note)

p_pc :: Pitch.PitchClass -> A.Parser Pitch.PitchClass
p_pc pc_per_oct = p_digit 1 (pc_per_oct+1)

p_octave :: A.Parser Pitch.Octave
p_octave = p_digit 0 10

p_digit :: Int -> Int -> A.Parser Int
p_digit low high = do
    n <- maybe mzero pure . Num.readDigit =<< A.satisfy ParseText.is_digit
    guard $ Num.inRange low high n
    return n
