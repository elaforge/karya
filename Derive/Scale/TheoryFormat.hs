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


-- | The usual 7 note scale, which wraps around at @c@ instead of @a@.
absolute_c :: PitchFormat
absolute_c =
    make_absolute_format (make_degrees absolute_c_degrees) ascii_accidentals

absolute_c_degrees :: [Text]
absolute_c_degrees = ["c", "d", "e", "f", "g", "a", "b"]

absolute_c_intervals :: [Int]
absolute_c_intervals = [2, 2, 1, 2, 2, 2, 1]

sargam :: ParseKey -> Theory.PitchClass -> PitchFormat
sargam = make_relative_format degrees ascii_accidentals
    where degrees = make_degrees ["s", "r", "g", "m", "p", "d", "n"]

cipher :: ParseKey -> Theory.PitchClass -> PitchFormat
cipher = make_relative_format degrees ascii_accidentals
    where degrees = make_degrees ["/1", "/2", "/3", "/4", "/5", "/6", "/7"]

chinese :: ParseKey -> Theory.PitchClass -> PitchFormat
chinese = make_relative_format degrees ascii_accidentals
    where degrees = make_degrees ["一", "二", "三", "四", "五", "六", "七"]

nanguan :: ParseKey -> Theory.PitchClass -> PitchFormat
nanguan = make_relative_format degrees ascii_accidentals
    where degrees = make_degrees ["士", "下", "ㄨ", "工", "六"]

-- * PitchFormat

data PitchFormat = PitchFormat {
    -- | Return the note and a possible adjustment for the octave.
    fmt_show :: Maybe Pitch.Key -> Theory.Note -> (Theory.Octave, Text)
    -- | This doesn't need the key because that work is split off to
    -- 'AdjustPitch'.
    , fmt_read :: A.Parser Theory.Note
    -- | I need the key to parse the pitch, but that can't happen unless I want
    -- to give all pattern_lookup calls access to the env in scope.  But
    -- I don't need the env to recognize if it's a valid call or not.
    , fmt_adjust :: Maybe Pitch.Key -> Theory.Pitch
        -> Either Scale.ScaleError Theory.Pitch
    }
type ParseKey = Maybe Pitch.Key -> Either Scale.ScaleError Theory.PitchClass
type Degrees = Vector.Vector Text

make_degrees :: [Text] -> Degrees
make_degrees = Vector.fromList

-- ** show, read

show_key :: PitchFormat -> Theory.Key -> Pitch.Key
show_key fmt key = Pitch.Key $
    snd (fmt_show fmt Nothing (Theory.key_tonic key))
        <> "-" <> Theory.key_name key

type ShowPitch = Maybe Pitch.Key -> Theory.Pitch -> Pitch.Note

show_pitch :: PitchFormat -> ShowPitch
show_pitch fmt key (Theory.Pitch oct note) =
    Pitch.Note $ show_octave (oct + octave) <> note_name
    where (octave, note_name) = fmt_show fmt key note

read_pitch :: PitchFormat -> Pitch.Note -> Either Scale.ScaleError Theory.Pitch
read_pitch fmt = maybe (Left Scale.UnparseableNote) Right
    . ParseBs.maybe_parse_text (Theory.Pitch <$> p_octave <*> fmt_read fmt)
    . Pitch.note_text

read_note :: PitchFormat -> Text -> Maybe Theory.Note
read_note fmt = ParseBs.maybe_parse_text (fmt_read fmt)

show_octave :: Theory.Octave -> Text
show_octave = showt . (subtract 1)

p_octave :: A.Parser Theory.Octave
p_octave = (+1) <$> ParseBs.p_int

-- ** make

make_absolute_format :: Degrees -> AccidentalFormat -> PitchFormat
make_absolute_format degrees acc_fmt =
    PitchFormat (const $ show_note_absolute degrees acc_fmt)
        (p_pitch_absolute degrees acc_fmt) (const Right)

make_relative_format :: Degrees -> AccidentalFormat -> ParseKey
    -> Theory.PitchClass
    -- ^ Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    -> PitchFormat
make_relative_format degrees acc_fmt parse_key default_key =
    PitchFormat p_show p_read p_adjust
    where
    p_show key = show_note_relative degrees acc_fmt
        (either (const default_key) id (parse_key key))
    p_read = p_pitch_relative degrees acc_fmt
    p_adjust maybe_key pitch = do
        key <- parse_key maybe_key
        return $ adjust_relative_key degrees key pitch

-- *** absolute

show_note_absolute :: Degrees -> AccidentalFormat -> Theory.Note
    -> (Theory.Octave, Text)
show_note_absolute degrees acc_fmt (Theory.Note pc acc) =
    (oct, (degrees Vector.! degree) <> show_accidentals acc_fmt acc)
    where (oct, degree) = pc `divMod` Vector.length degrees

p_pitch_absolute :: Degrees -> AccidentalFormat -> A.Parser Theory.Note
p_pitch_absolute = p_pitch_relative

-- *** relative

-- | Like 'show_note_absolute', this subtracts 2 from the octave so middle
-- C winds up at octave 4.
show_note_relative :: Degrees -> AccidentalFormat -> Theory.PitchClass
    -> Theory.Note -> (Theory.Octave, Text)
show_note_relative degrees acc_fmt key (Theory.Note pc acc) =
    (oct, (degrees Vector.! degree) <> show_accidentals acc_fmt acc)
    where (oct, degree) = (pc - key) `divMod` Vector.length degrees

p_pitch_relative :: Degrees -> AccidentalFormat -> A.Parser Theory.Note
p_pitch_relative degrees acc_fmt =
    Theory.Note <$> p_degree <*> p_accidentals acc_fmt
    where
    -- TODO this is inefficient, I should switch to Text attoparsec some day
    p_degree = A.choice
        [ A.string (Encoding.encodeUtf8 s) >> return i
        | (i, s) <- zip [0..] (Vector.toList degrees)
        ]

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
