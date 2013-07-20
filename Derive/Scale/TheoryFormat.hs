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
import qualified Data.Vector.Unboxed as Unboxed

import Util.Control
import qualified Util.ParseBs as ParseBs
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Perform.Pitch as Pitch


-- * absolute

-- | Make an absolute scale starting at @a@.
letters :: Theory.PitchClass -> Format
letters pc_per_octave =
    make_absolute_format (make_pattern degrees) degrees ascii_accidentals
    where
    degrees = make_degrees $ map Text.singleton $ take pc_per_octave ['a'..'z']

-- | The usual 7 note scale, which wraps around at @c@ instead of @a@.
absolute_c :: Format
absolute_c =
    make_absolute_format (make_pattern degrees) degrees ascii_accidentals
    where degrees = make_degrees absolute_c_degrees

absolute_c_degrees :: [Text]
absolute_c_degrees = ["c", "d", "e", "f", "g", "a", "b"]

piano_intervals :: [Int]
piano_intervals = [2, 2, 1, 2, 2, 2, 1]

-- | The layout of keys on everyone's favorite boxed harp.
piano_layout :: Theory.Layout
piano_layout = Theory.layout piano_intervals

-- * relative

-- | Args for a relative scale format.
data RelativeFormat key = RelativeFormat {
    rel_acc_fmt :: AccidentalFormat
    , rel_parse_key :: ParseKey key
    -- Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    , rel_default_key :: key
    , rel_show_note :: ShowNote key
    , rel_to_absolute :: ToAbsolute key
    }

sargam :: RelativeFormat key -> Format
sargam = make_relative_format (make_pattern degrees) degrees
    where degrees = make_degrees ["s", "r", "g", "m", "p", "d", "n"]

cipher :: Theory.PitchClass -> RelativeFormat key -> Format
cipher pc_per_octave = make_relative_format pattern degrees
    where
    degrees = make_degrees ["/" <> showt pc | pc <- [1..pc_per_octave]]
    pattern = "/[1-" <> showt pc_per_octave <> "]"

zh_cipher :: Theory.PitchClass -> RelativeFormat key -> Format
zh_cipher pc_per_octave =
    make_relative_format (make_pattern degrees) degrees
    where
    degrees = make_degrees $ take pc_per_octave ds
    ds = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

gongche :: RelativeFormat key -> Format
gongche = make_relative_format (make_pattern degrees) degrees
    where degrees = make_degrees ["士", "下", "ㄨ", "工", "六"]

make_pattern :: Degrees -> Text
make_pattern degrees = "[" <> mconcat (Vector.toList degrees) <> "]"

-- * Format

data Format = Format {
    -- | Return the note and a possible adjustment for the octave.
    fmt_show :: Maybe Pitch.Key -> Theory.Note -> (Theory.Octave, Text)
    -- | This doesn't need the key because that work is split off to
    -- 'fmt_to_absolute'.
    , fmt_read :: A.Parser Theory.Note
    -- | I need the key to parse the pitch, but that can't happen unless I want
    -- to give all pattern_lookup calls access to the env in scope.  But
    -- I don't need the env to recognize if it's a valid call or not.
    , fmt_to_absolute :: Maybe Pitch.Key -> Theory.Pitch
        -> Either Scale.ScaleError Theory.Pitch
    , fmt_pattern :: !Text
    , fmt_pc_per_octave :: Theory.PitchClass
    }
type ParseKey key = Maybe Pitch.Key -> Either Scale.ScaleError key
type Degrees = Vector.Vector Text

make_degrees :: [Text] -> Degrees
make_degrees = Vector.fromList

-- ** show keys

show_key :: Format -> Theory.Key -> Pitch.Key
show_key fmt key = Pitch.Key $
    tonic <> (if Text.null name then "" else "-" <> name)
    where
    tonic = snd $ fmt_show fmt Nothing (Theory.key_tonic key)
    name = Theory.key_name key

-- | Show a key along with its key signature.
show_key_signature :: Format -> Theory.Key -> Text
show_key_signature fmt key =
    Pitch.key_text (show_key fmt key) <> " -- " <> intervals
        <> "\n    " <> commas (show_signature fmt key)
    where
    intervals = commas $ map showt $ Unboxed.toList $ Theory.key_intervals key
    commas = Text.intercalate ", "

-- | Show the signature of the given key by showing each scale degree with the
-- the accidentals implied by the key signature.
show_signature :: Format -> Theory.Key -> [Text]
show_signature fmt key = map (show_note fmt Nothing . Theory.pitch_note) pitches
    where
    pc_per_octave = Unboxed.length (Theory.key_intervals key)
    pitches = map transpose [0 .. pc_per_octave - 1]
    transpose pc = Theory.transpose_diatonic key pc
        (Theory.Pitch 0 (Theory.key_tonic key))

-- | Get the note names of a chromatic scale in this key.
key_notes :: Theory.Key -> Format -> [Text]
key_notes key fmt = map (show_note fmt Nothing . Theory.pitch_note) pitches
    where
    per_octave = Theory.layout_semis_per_octave (Theory.key_layout key)
    pitches = map transpose [0 .. per_octave - 1]
    transpose steps = Theory.transpose_chromatic key steps
        (Theory.Pitch 0 (Theory.Note 0 0))

-- ** show pitches

type ShowPitch = Maybe Pitch.Key -> Theory.Pitch -> Pitch.Note

show_pitch :: Format -> ShowPitch
show_pitch fmt key (Theory.Pitch oct note) =
    Pitch.Note $ show_octave (oct + octave) <> note_name
    where (octave, note_name) = fmt_show fmt key note

show_note :: Format -> Maybe Pitch.Key -> Theory.Note -> Text
show_note fmt key = snd . fmt_show fmt key

read_pitch :: Format -> Pitch.Note -> Either Scale.ScaleError Theory.Pitch
read_pitch fmt = maybe (Left Scale.UnparseableNote) Right
    . ParseBs.maybe_parse_text (Theory.Pitch <$> p_octave <*> fmt_read fmt)
    . Pitch.note_text

read_note :: Format -> Text -> Maybe Theory.Note
read_note fmt = ParseBs.maybe_parse_text (fmt_read fmt)

-- | This subtracts 1 from the octave so middle C winds up at octave 4, as per
-- 'Theory.Octave'.
show_octave :: Theory.Octave -> Text
show_octave = showt . (subtract 1)

p_octave :: A.Parser Theory.Octave
p_octave = (+1) <$> ParseBs.p_int

-- ** make

make_absolute_format :: Text -> Degrees -> AccidentalFormat -> Format
make_absolute_format pattern degrees acc_fmt = Format
    { fmt_show = const $ show_note_absolute degrees acc_fmt
    , fmt_read = p_pitch_absolute degrees acc_fmt
    , fmt_to_absolute = const Right
    , fmt_pattern = octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = Vector.length degrees
    }

make_relative_format :: Text -> Degrees -> RelativeFormat key -> Format
make_relative_format pattern degrees
        (RelativeFormat acc_fmt parse_key default_key show_note to_abs) =
    Format
        { fmt_show = p_show
        , fmt_read = p_read
        , fmt_to_absolute = p_absolute
        , fmt_pattern = octave_pattern <> pattern <> acc_pattern
        , fmt_pc_per_octave = Vector.length degrees
        }
    where
    p_show key = show_note degrees acc_fmt
        (either (const default_key) id (parse_key key))
    p_read = p_pitch_relative degrees acc_fmt
    p_absolute maybe_key pitch = do
        key <- parse_key maybe_key
        return $ to_abs degrees key pitch

type ShowNote key = Degrees -> AccidentalFormat -> key -> Theory.Note
    -> (Theory.Octave, Text)

-- | Given a relative pitch relative to the default key, adjust it to
-- be absolute.  This is so I can figure out if a relative pitch is valid
-- without knowing the key, as described in 'fmt_to_absolute'.
type ToAbsolute key = Degrees -> key -> Theory.Pitch -> Theory.Pitch

acc_pattern :: Text
acc_pattern = "(#|x|b|bb)?"

octave_pattern :: Text
octave_pattern = "[-1-9]"

-- *** absolute

show_note_absolute :: Degrees -> AccidentalFormat -> Theory.Note
    -> (Theory.Octave, Text)
show_note_absolute degrees acc_fmt (Theory.Note pc acc) =
    (oct, (degrees Vector.! degree) <> show_accidentals acc_fmt acc)
    where (oct, degree) = pc `divMod` Vector.length degrees

p_pitch_absolute :: Degrees -> AccidentalFormat -> A.Parser Theory.Note
p_pitch_absolute = p_pitch_relative

-- *** relative

show_note_chromatic :: ShowNote Theory.Key
show_note_chromatic degrees acc_fmt key (Theory.Note pc acc) =
    (oct, text <> acc_text)
    where
    acc_text = show_accidentals acc_fmt
        (acc - Theory.note_accidentals tonic)
    (oct, text) = show_degree degrees (Theory.note_pc tonic) pc
    tonic = Theory.key_tonic key

chromatic_to_absolute :: ToAbsolute Theory.Key
chromatic_to_absolute degrees key (Theory.Pitch octave (Theory.Note pc acc)) =
    Theory.Pitch (octave + oct) (Theory.Note pc2 acc2)
    where
    (oct, pc2) = (pc + Theory.note_pc tonic) `divMod` Vector.length degrees
    acc2 = acc + case Theory.key_signature key of
        -- If it's chromatic then I can't adjust for the mode, but I still
        -- want to map degree 1 to C# if I'm in C#.
        Nothing -> Theory.note_accidentals tonic
        Just sig -> case sig Unboxed.!? pc of
            Nothing -> 0 -- That shouldn't have happened.
            Just acc -> acc
    tonic = Theory.key_tonic key

show_note_diatonic :: ShowNote Theory.PitchClass
show_note_diatonic degrees acc_fmt key (Theory.Note pc acc) =
    (oct, text <> show_accidentals acc_fmt acc)
    where (oct, text) = show_degree degrees key pc

show_degree :: Degrees -> Theory.PitchClass -> Theory.PitchClass
    -> (Theory.Octave, Text)
show_degree degrees key pc = (oct, degrees Vector.! degree)
    where (oct, degree) = (pc - key) `divMod` Vector.length degrees

diatonic_to_absolute :: ToAbsolute Theory.PitchClass
diatonic_to_absolute degrees key (Theory.Pitch octave (Theory.Note pc acc)) =
    Theory.Pitch (octave + oct) (Theory.Note pc2 acc)
    where (oct, pc2) = (pc + key) `divMod` Vector.length degrees

p_pitch_relative :: Degrees -> AccidentalFormat -> A.Parser Theory.Note
p_pitch_relative degrees acc_fmt =
    Theory.Note <$> p_degree <*> p_accidentals acc_fmt
    where
    -- TODO this is inefficient, I should switch to Text attoparsec some day
    p_degree = A.choice
        [ A.string (Encoding.encodeUtf8 s) >> return i
        | (i, s) <- zip [0..] (Vector.toList degrees)
        ]


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
