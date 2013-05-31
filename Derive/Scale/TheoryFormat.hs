-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is the part of "Derive.Scale.Theory" that's concerned with converting
-- 'Theory.Pitch'es to and from 'Pitch.Note's.
--
-- It's split off to avoid cluttering Theory, but also because the
-- "Derive.Scale" import would make it a circular dependency.
module Derive.Scale.TheoryFormat where
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Perform.Pitch as Pitch


-- * absolute pitches

-- | Parse a Pitch and verify it against the given Layout.  Octaves wrap at
-- A, which is nonstandard, the usual practice is to wrap at C.
read_pitch :: Theory.Layout -> Pitch.Note -> Maybe Theory.Pitch
read_pitch layout note = case parse_pitch note of
    Just p | Theory.note_in_layout layout (Theory.pitch_note p) -> Just p
    _ -> Nothing

-- | Parse a Note and verify it against the given Layout.
read_note :: Theory.Layout -> Text -> Maybe Theory.Note
read_note layout s = case parse_note ascii_accidentals s of
    Just n | Theory.note_in_layout layout n -> Just n
    _ -> Nothing

-- | Show and read pitches in the usual letter format.  The inverse of
-- 'p_pitch'.
--
-- This subtracts 2 from the octave so that middle C winds up at octave 4, and
-- the bottom of the range ends up at octave -1.
show_pitch :: AccidentalFormat -> Theory.Pitch -> Pitch.Note
show_pitch acc_format pitch =
    Pitch.Note $ showt (oct - 2) <> show_note acc_format note
    where (oct, note) = Theory.pitch_c_octave pitch

parse_pitch :: Pitch.Note -> Maybe Theory.Pitch
parse_pitch =
    ParseBs.maybe_parse_text (p_pitch ascii_accidentals) . Pitch.note_text

-- | Internally octaves wrap at A, but the text representation wraps at C,
-- since that's how the rest of the world does it.
p_pitch :: AccidentalFormat -> A.Parser Theory.Pitch
p_pitch acc_format = do
    oct <- ParseBs.p_int
    note <- p_note acc_format
    let octave = if Theory.note_pc note >= 2 then oct - 1 else oct
    return $ Theory.Pitch (octave + 2) note

show_note :: AccidentalFormat -> Theory.Note -> Text
show_note acc_format (Theory.Note pc acc) =
    Text.cons (Theory.pc_char pc) (show_accidentals acc_format acc)

parse_note :: AccidentalFormat -> Text -> Maybe Theory.Note
parse_note acc_format = ParseBs.maybe_parse_text (p_note acc_format)

p_note :: AccidentalFormat -> A.Parser Theory.Note
p_note acc_format = do
    c <- A.satisfy (\c -> c >= 'a' && c <= 'z')
    accs <- p_accidentals acc_format
    return $ Theory.Note (Theory.char_pc c) accs


-- * relative pitches

absolute_format :: Theory.PitchClass -> PitchFormat
absolute_format pc_per_octave = (show_absolute, read_absolute, const Right)
    where
    show_absolute _ = show_pitch ascii_accidentals
    read_absolute note = case parse_pitch note of
        Just pitch | valid pitch -> Right pitch
        _ -> Left Scale.UnparseableNote
    valid pitch =
        Theory.note_pc note >= 0 && Theory.note_pc note < pc_per_octave
        where note = Theory.pitch_note pitch

sargam :: ParseKey -> Theory.PitchClass -> PitchFormat
sargam = make_relative_format degrees ascii_accidentals
    where degrees = Vector.fromList ["s", "r", "g", "m", "p", "d", "n"]

cipher :: ParseKey -> Theory.PitchClass -> PitchFormat
cipher = make_relative_format degrees ascii_accidentals
    where degrees = Vector.fromList ["-1", "-2", "-3", "-4", "-5", "-6", "-7"]

chinese :: ParseKey -> Theory.PitchClass -> PitchFormat
chinese = make_relative_format degrees ascii_accidentals
    where degrees = Vector.fromList ["一", "二", "三", "四", "五", "六", "七"]

nanguan :: ParseKey -> Theory.PitchClass -> PitchFormat
nanguan = make_relative_format degrees ascii_accidentals
    where degrees = Vector.fromList ["士", "下", "ㄨ", "工", "六"]

-- ** implementation

type PitchFormat = (ShowPitch, ReadPitch, AdjustPitch)
type ShowPitch = Maybe Pitch.Key -> Theory.Pitch -> Pitch.Note
type ReadPitch = Pitch.Note -> Either Scale.ScaleError Theory.Pitch
-- | I need the key to parse the pitch, but that can't happen unless I want to
-- give all pattern_lookup calls access to the env in scope.  But I don't need
-- the env to recognize if it's a valid call or not.
type AdjustPitch = Maybe Pitch.Key -> Theory.Pitch
    -> Either Scale.ScaleError Theory.Pitch
type ParseKey = Maybe Pitch.Key -> Either Scale.ScaleError Theory.PitchClass

type Degrees = Vector.Vector Text

make_relative_format :: Degrees -> AccidentalFormat
    -> ParseKey -> Theory.PitchClass
    -- ^ Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    -> PitchFormat
make_relative_format degrees acc_format parse_key default_key =
    (p_show, p_read, p_adjust)
    where
    p_show key = Pitch.Note . show_pitch_relative degrees acc_format
        (either (const default_key) id $ parse_key key)
    p_read = maybe (Left Scale.UnparseableNote) Right
        . ParseBs.maybe_parse_text (p_pitch_relative degrees acc_format)
        . Pitch.note_text
    p_adjust maybe_key pitch = do
        key <- parse_key maybe_key
        return $ adjust_relative_key degrees key pitch

-- | Like 'show_pitch', this subtracts 1 from the octave so middle C winds up
-- at octave 4.
show_pitch_relative :: Degrees -> AccidentalFormat -> Theory.PitchClass
    -> Theory.Pitch -> Text
show_pitch_relative degrees acc_format key
        (Theory.Pitch octave (Theory.Note pc acc)) =
    showt (octave + oct - 1) <> (degrees Vector.! degree)
        <> show_accidentals acc_format acc
    where (oct, degree) = (pc - key) `divMod` Vector.length degrees

p_pitch_relative :: Degrees -> AccidentalFormat -> A.Parser Theory.Pitch
p_pitch_relative degrees acc_format = do
    octave <- ParseBs.p_int
    -- TODO this is inefficient, I should switch to Text attoparsec some day
    degree <- A.choice
        [ A.string (Encoding.encodeUtf8 s) >> return i
        | (i, s) <- zip [0..] (Vector.toList degrees)
        ]
    acc <- p_accidentals acc_format
    return $ Theory.Pitch octave (Theory.Note degree acc)

adjust_relative_key :: Degrees -> Theory.PitchClass -> Theory.Pitch
    -> Theory.Pitch
adjust_relative_key degrees key (Theory.Pitch octave (Theory.Note pc acc)) =
    Theory.Pitch (octave + oct) (Theory.Note pc2 acc)
    where (oct, pc2) = (pc + key) `divMod` Vector.length degrees


-- * accidentals

-- | sharp1, sharp2, flat1, flat2
data AccidentalFormat = AccidentalFormat !Text !Text !Text !Text deriving (Show)

ascii_accidentals :: AccidentalFormat
ascii_accidentals = AccidentalFormat "#" "x" "b" "bb"

symbol_accidentals :: AccidentalFormat
symbol_accidentals = AccidentalFormat "`#`" "`##`" "`b`" "`bb`"

p_accidentals :: AccidentalFormat -> A.Parser Theory.Accidentals
p_accidentals (AccidentalFormat sharp1 sharp2 flat1 flat2) = do
    sum <$> ParseBs.many (A.choice [p_flat2, p_sharp2, p_flat1, p_sharp1])
    where
    p_sharp1 = A.string (bs sharp1) >> return 1
    p_sharp2 = A.string (bs sharp2) >> return 2
    p_flat1 = A.string (bs flat1) >> return (-1)
    p_flat2 = A.string (bs flat2) >> return (-2)
    bs = Encoding.encodeUtf8

show_accidentals :: AccidentalFormat -> Theory.Accidentals -> Text
show_accidentals (AccidentalFormat sharp1 sharp2 flat1 flat2) acc
    | acc == 0 = ""
    | acc < 0 = Text.replicate x flat2 <> Text.replicate s flat1
    | otherwise = Text.replicate x sharp2 <> Text.replicate s sharp1
    where (x, s) = abs acc `divMod` 2
