-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-| This is the part of "Derive.Scale.Theory" that's concerned with converting
    'Pitch.Pitch'es to and from 'Pitch.Note's.

    It's split off to avoid cluttering Theory, but also because the
    "Derive.Scale" import would make it a circular dependency.
-}
module Derive.Scale.TheoryFormat where
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as Unboxed

import Util.Control
import qualified Util.ParseText as ParseText
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Perform.Pitch as Pitch


-- * absolute

-- | Make an absolute scale starting at @a@.
letters :: Pitch.PitchClass -> Format
letters pc_per_octave =
    make_absolute_format (make_pattern degrees) degrees
    where degrees = make_degrees $ take pc_per_octave letter_degrees

letter_degrees :: [Text]
letter_degrees = map Text.singleton ['a'..'z']

-- | The usual 7 note scale, which wraps around at @c@ instead of @a@.
absolute_c :: Format
absolute_c =
    make_absolute_format (make_pattern degrees) degrees
    where degrees = make_degrees absolute_c_degrees

absolute_c_degrees :: [Text]
absolute_c_degrees = ["c", "d", "e", "f", "g", "a", "b"]

-- * relative

-- | Args for a relative scale format.
--
-- The 'Pitch.Pitch'es handled by a relative scale are still absolute, exactly
-- the same as the Pitches of an absolute scale.  The difference is that the
-- 'read_pitch' and 'show_pitch' functions adjust based on the key to display
-- the absolute Pitch relative to the tonic of the key.
data RelativeFormat key = RelativeFormat {
    rel_acc_fmt :: AccidentalFormat
    , rel_parse_key :: ParseKey key
    -- Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    , rel_default_key :: key
    , rel_show_degree :: ShowDegree key
    , rel_to_absolute :: ToAbsolute key
    , rel_key_tonic :: key -> Pitch.PitchClass
    }
type ShowDegree key = key -> ShowOctave -> Degrees -> AccidentalFormat
    -> Either Pitch.Degree Pitch.Pitch -> Pitch.Note

sargam :: Show key => RelativeFormat key -> Format
sargam = make_relative_format (make_pattern degrees) degrees
    where degrees = make_degrees ["s", "r", "g", "m", "p", "d", "n"]

cipher :: Show key => Pitch.PitchClass -> RelativeFormat key -> Format
cipher pc_per_octave = make_relative_format pattern degrees
    where
    degrees = make_degrees ["-" <> showt pc | pc <- [1..pc_per_octave]]
    pattern = "-[1-" <> showt pc_per_octave <> "]"

zh_cipher :: Show key => Pitch.PitchClass -> RelativeFormat key -> Format
zh_cipher pc_per_octave =
    make_relative_format (make_pattern degrees) degrees
    where
    degrees = make_degrees $ take pc_per_octave ds
    ds = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

gongche :: Show key => RelativeFormat key -> Format
gongche = make_relative_format (make_pattern degrees) degrees
    where degrees = make_degrees ["士", "下", "ㄨ", "工", "六"]

make_pattern :: Degrees -> Text
make_pattern degrees = "[" <> mconcat (Vector.toList degrees) <> "]"

-- * Format

data Format = Format {
    fmt_show :: ShowPitch
    -- | This doesn't need the key because that work is split off to
    -- 'fmt_to_absolute'.
    , fmt_read :: A.Parser Pitch.Pitch
    -- | I need the key to parse the pitch, but that can't happen unless I want
    -- to give all pattern_lookup calls access to the env in scope.  But
    -- I don't need the env to recognize if it's a valid call or not.
    , fmt_to_absolute :: Maybe Pitch.Key -> Pitch.Pitch
        -> Either Scale.ScaleError Pitch.Pitch
    , fmt_key_tonic :: Maybe Pitch.Key -> Maybe Pitch.PitchClass
    , fmt_pattern :: !Text
    , fmt_pc_per_octave :: Pitch.PitchClass
    -- | True if this scale is relative to the key.
    , fmt_relative :: !Bool
    }
type ShowPitch = Maybe Pitch.Key -> Either Pitch.Degree Pitch.Pitch
    -> Pitch.Note
type ParseKey key = Maybe Pitch.Key -> Either Scale.ScaleError key
type Degrees = Vector.Vector Text

make_degrees :: [Text] -> Degrees
make_degrees = Vector.fromList

-- ** show keys

show_key :: Format -> Theory.Key -> Pitch.Key
show_key fmt key =
    Pitch.Key $ tonic <> (if Text.null name then "" else "-" <> name)
    where
    tonic = show_degree fmt Nothing (Theory.key_tonic key)
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
show_signature fmt key =
    map (show_degree fmt Nothing . Pitch.pitch_degree) pitches
    where
    pc_per_octave = Unboxed.length (Theory.key_intervals key)
    pitches = map transpose [0 .. pc_per_octave - 1]
    transpose pc = Theory.transpose_diatonic key pc
        (Pitch.Pitch 0 (Theory.key_tonic key))

-- | Get the degree names of a chromatic scale in this key.
key_degrees :: Theory.Key -> Format -> [Text]
key_degrees key fmt = map (show_degree fmt Nothing . Pitch.pitch_degree) pitches
    where
    per_octave = Theory.layout_semis_per_octave (Theory.key_layout key)
    pitches = map transpose [0 .. per_octave - 1]
    transpose steps = Theory.transpose_chromatic key steps
        (Pitch.Pitch 0 (Pitch.Degree 0 0))

-- ** show pitches

show_pitch :: Format -> Maybe Pitch.Key -> Pitch.Pitch -> Pitch.Note
show_pitch fmt key = fmt_show fmt key . Right

-- | 'show_pitch' adapted to 'Scale.scale_show'.
scale_show_pitch :: Format -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either Scale.ScaleError Pitch.Note
scale_show_pitch fmt key = Right . show_pitch fmt key

show_degree :: Format -> Maybe Pitch.Key -> Pitch.Degree -> Text
show_degree fmt key = Pitch.note_text . fmt_show fmt key . Left

read_pitch :: Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Pitch.Pitch
read_pitch fmt key = fmt_to_absolute fmt key <=< read_unadjusted_pitch fmt

-- | Parse a Note, but don't adjust it for the key.  This means that relative
-- pitches will likely be incorrect.  'ToAbsolute' documents why this needs
-- to be separate.
read_unadjusted_pitch :: Format -> Pitch.Note
    -> Either Scale.ScaleError Pitch.Pitch
read_unadjusted_pitch fmt = maybe (Left Scale.UnparseableNote) Right
    . ParseText.maybe_parse (fmt_read fmt)
    . Pitch.note_text

-- ** make

make_absolute_format :: Text -> Degrees -> Format
make_absolute_format =
    make_absolute_format_config default_octave_format ascii_accidentals

-- | A configurable version of 'make_absolute_format'.
make_absolute_format_config :: OctaveFormat -> AccidentalFormat
    -> Text -> Degrees -> Format
make_absolute_format_config (show_oct, parse_oct) acc_fmt pattern degrees =
        Format
    { fmt_show = show_pitch_absolute show_oct degrees acc_fmt
    , fmt_read = p_pitch_absolute parse_oct degrees acc_fmt
    , fmt_to_absolute = const Right
    , fmt_key_tonic = const Nothing
    , fmt_pattern = octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = Vector.length degrees
    , fmt_relative = False
    }

make_relative_format :: Show key => Text -> Degrees -> RelativeFormat key
    -> Format
make_relative_format = make_relative_format_config default_octave_format

make_relative_format_config :: Show key => OctaveFormat -> Text -> Degrees
    -> RelativeFormat key -> Format
make_relative_format_config (show_oct, parse_oct) pattern degrees
        (RelativeFormat acc_fmt parse_key default_key show_degree to_abs
            key_tonic) =
    Format
        { fmt_show = p_show
        , fmt_read = p_read
        , fmt_to_absolute = p_absolute
        , fmt_key_tonic = p_tonic
        , fmt_pattern = octave_pattern <> pattern <> acc_pattern
        , fmt_pc_per_octave = Vector.length degrees
        , fmt_relative = True
        }
    where
    p_show key = show_degree (either (const default_key) id (parse_key key))
        show_oct degrees acc_fmt
    p_read = p_pitch_relative parse_oct degrees acc_fmt
    p_absolute maybe_key pitch = do
        key <- parse_key maybe_key
        return $ to_abs key degrees pitch
    p_tonic maybe_key =
        key_tonic <$> either (const Nothing) Just (parse_key maybe_key)

-- | Given a relative pitch relative to the default key, adjust it to
-- be absolute.  This is so I can figure out if a relative pitch is valid
-- without knowing the key, as described in 'fmt_to_absolute'.
type ToAbsolute key = key -> Degrees -> Pitch.Pitch -> Pitch.Pitch

type Tonic = Pitch.PitchClass

acc_pattern :: Text
acc_pattern = "(#|x|b|bb)?"

octave_pattern :: Text
octave_pattern = "[-1-9]"

-- *** absolute

show_pitch_absolute :: ShowOctave -> Degrees -> AccidentalFormat -> ShowPitch
show_pitch_absolute show_octave degrees acc_fmt _key pitch =
    Pitch.Note $ case pitch of
        Left (Pitch.Degree pc acc) ->
            degrees ! (pc `mod` Vector.length degrees)
                <> show_accidentals acc_fmt acc
        Right (Pitch.Pitch oct (Pitch.Degree pc_ acc)) ->
            show_octave (oct + pc_oct) $
                degrees ! pc <> show_accidentals acc_fmt acc
            where (pc_oct, pc) = pc_ `divMod` Vector.length degrees

p_pitch_absolute :: ParseOctave -> Degrees -> AccidentalFormat
    -> A.Parser Pitch.Pitch
p_pitch_absolute = p_pitch_relative

-- *** relative

show_degree_chromatic :: ShowDegree Theory.Key
show_degree_chromatic key show_octave degrees acc_fmt degree_pitch =
    Pitch.Note $ case degree_pitch of
        Left _ -> pc_text <> acc_text
        Right (Pitch.Pitch oct _) ->
            show_octave (oct + pc_oct) (pc_text <> acc_text)
    where
    Pitch.Degree pc acc = either id Pitch.pitch_degree degree_pitch
    acc_text = show_accidentals acc_fmt $ acc - Theory.accidentals_at_pc key pc
    (pc_oct, pc_text) =
        show_pc degrees (Pitch.degree_pc (Theory.key_tonic key)) pc

chromatic_to_absolute :: ToAbsolute Theory.Key
chromatic_to_absolute key degrees (Pitch.Pitch octave (Pitch.Degree pc acc)) =
    Pitch.Pitch (octave + oct) (Pitch.Degree pc2 acc2)
    where
    (oct, pc2) = (pc + Pitch.degree_pc tonic) `divMod` Vector.length degrees
    acc2 = acc + case Theory.key_signature key of
        -- If it's chromatic then I can't adjust for the mode, but I still
        -- want to map degree 1 to C# if I'm in C#.
        Nothing -> Pitch.degree_accidentals tonic
        Just sig -> case sig Unboxed.!? pc of
            Nothing -> 0 -- That shouldn't have happened.
            Just acc -> acc
    tonic = Theory.key_tonic key

show_degree_diatonic :: ShowDegree Tonic
show_degree_diatonic tonic show_octave degrees acc_fmt degree_pitch =
    Pitch.Note $ case degree_pitch of
        Left _ -> pc_text <> acc_text
        Right (Pitch.Pitch oct _) ->
            show_octave (oct + pc_oct) (pc_text <> acc_text)
    where
    Pitch.Degree pc acc = either id Pitch.pitch_degree degree_pitch
    acc_text = show_accidentals acc_fmt acc
    (pc_oct, pc_text) = show_pc degrees tonic pc

show_pc :: Degrees -> Tonic -> Pitch.PitchClass -> (Pitch.Octave, Text)
show_pc degrees tonic pc = (oct, degrees ! degree)
    where (oct, degree) = (pc - tonic) `divMod` Vector.length degrees

diatonic_to_absolute :: ToAbsolute Tonic
diatonic_to_absolute tonic degrees (Pitch.Pitch octave (Pitch.Degree pc acc)) =
    Pitch.Pitch (octave + oct) (Pitch.Degree pc2 acc)
    where (oct, pc2) = (pc + tonic) `divMod` Vector.length degrees

p_pitch_relative :: ParseOctave -> Degrees -> AccidentalFormat
    -> A.Parser Pitch.Pitch
p_pitch_relative parse_octave degrees acc_fmt =
    parse_octave $ Pitch.Degree <$> p_degree <*> p_accidentals acc_fmt
    where
    p_degree = A.choice
        [ A.string text >> return i
        | (i, text) <- zip [0..] (Vector.toList degrees)
        ]

-- * octave

-- | Configure how the octave portion of the 'Pitch.Pitch' is shown.
type OctaveFormat = (ShowOctave, ParseOctave)
type ShowOctave = Pitch.Octave -> Text -> Text
type ParseOctave = A.Parser Pitch.Degree -> A.Parser Pitch.Pitch

-- Most scales display the octave as a leading number.
default_octave_format :: OctaveFormat
default_octave_format = (show_octave, parse_octave)

show_octave :: ShowOctave
show_octave oct = (showt oct <>)

parse_octave :: ParseOctave
parse_octave p_degree = Pitch.Pitch <$> ParseText.p_int <*> p_degree

-- * accidentals

-- | sharp1, sharp2, flat1, flat2
data AccidentalFormat = AccidentalFormat !Text !Text !Text !Text deriving (Show)

ascii_accidentals :: AccidentalFormat
ascii_accidentals = AccidentalFormat "#" "x" "b" "bb"

symbol_accidentals :: AccidentalFormat
symbol_accidentals = AccidentalFormat "`#`" "`##`" "`b`" "`bb`"

p_accidentals :: AccidentalFormat -> A.Parser Pitch.Accidentals
p_accidentals (AccidentalFormat sharp1 sharp2 flat1 flat2) = do
    sum <$> ParseText.many (A.choice [p_flat2, p_sharp2, p_flat1, p_sharp1])
    where
    p_sharp1 = A.string sharp1 >> return 1
    p_sharp2 = A.string sharp2 >> return 2
    p_flat1 = A.string flat1 >> return (-1)
    p_flat2 = A.string flat2 >> return (-2)

show_accidentals :: AccidentalFormat -> Pitch.Accidentals -> Text
show_accidentals (AccidentalFormat sharp1 sharp2 flat1 flat2) acc
    | acc == 0 = ""
    | acc < 0 = Text.replicate x flat2 <> Text.replicate s flat1
    | otherwise = Text.replicate x sharp2 <> Text.replicate s sharp1
    where (x, s) = abs acc `divMod` 2
