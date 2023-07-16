-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This is the part of "Derive.Scale.Theory" that's concerned with converting
    'Pitch.Pitch'es to and from 'Pitch.Note's.

    It's split off to avoid cluttering Theory, but also because the
    "Derive.Scale" import would make it a circular dependency.

    This is basically just a bunch of functions that take a million arguments
    to configure octave format, accidental format, key parsing, etc.  To
    avoid annoyingly long argument lists, they are mostly packaged up into
    records, such as 'Config', 'KeyConfig', and 'RelativeFormat'.

    It's basically all just an attempt to parameterize scale creation, so I
    can reuse the shared parts but still be able to configure them.  It winds
    up being complicated, probably due to how it's evolved in reaction to
    increasingly varied scales, rather than according to an overarching design.
-}
module Derive.Scale.TheoryFormat where
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           Data.Vector ((!))

import qualified Util.Debug as Debug
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale.Theory as Theory
import qualified Perform.Pitch as Pitch

import           Global


-- * types

-- | General purpose config.
data Config = Config {
    config_show_octave :: ShowOctave
    , config_parse_octave :: ParseOctave
    , config_accidental :: AccidentalFormat
    }

default_config :: Config
default_config = Config
    { config_show_octave = show_octave
    , config_parse_octave = parse_octave
    , config_accidental = ascii_accidentals
    }

set_octave :: ShowOctave -> ParseOctave -> Config -> Config
set_octave show_octave parse_octave config = config
    { config_show_octave = show_octave
    , config_parse_octave = parse_octave
    }

type ShowOctave = Pitch.Octave -> Text -> Text
-- | This can't just be A.Parser Pitch.Octave because I don't know where the
-- octave is in the pitch text.
type ParseOctave = A.Parser (Pitch.PitchClass, Maybe Pitch.Accidentals)
    -> A.Parser RelativePitch

-- | Key config is only necessary for formatting that depends on the key, e.g.
-- 'RelativeFormat'.
data KeyConfig key = KeyConfig {
    key_parse :: Maybe Pitch.Key -> Either DeriveT.PitchError key
    -- | Default key if there is none, or it's not parseable.  Otherwise, a bad
    -- or missing key would mean you couldn't even display notes.
    , key_default :: key
    }

-- | This is a just-parsed pitch.  It hasn't yet been adjusted according to the
-- key, so it's not yet an absolute 'Pitch.Pitch'.  It also represents
-- a natural explicitly, via Just 0.
--
-- 'fmt_to_absolute' is responsible for converting this to a 'Pitch.Pitch',
-- likely via 'rel_to_absolute'.
data RelativePitch =
    RelativePitch !Pitch.Octave !Pitch.PitchClass !(Maybe Pitch.Accidentals)
    deriving (Show)

instance Pretty RelativePitch where
    pretty (RelativePitch oct pc accs) = "relative:"
        <> showt oct <> "-" <> showt pc <> "-" <> pretty accs

relative_to_absolute :: RelativePitch -> Pitch.Pitch
relative_to_absolute (RelativePitch oct pc acc) =
    Pitch.Pitch oct (Pitch.Degree pc (fromMaybe 0 acc))

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
absolute_c = make_absolute_format (make_pattern degrees) degrees
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
    rel_config :: Config
    , rel_key_config :: KeyConfig key
    , rel_show_degree :: ShowDegree key
    , rel_to_absolute :: ToAbsolute key
    }

-- | Given a RelativePitch relative to the default key, adjust it to
-- be absolute.  This is so I can figure out if a relative pitch is valid
-- without knowing the key, as described in 'fmt_to_absolute'.
type ToAbsolute key = key -> Degrees -> RelativePitch -> Pitch.Pitch

-- | This is a specialization of 'ShowPitch' for show functions that need
-- a key.
type ShowDegree key = key -> ShowOctave -> Degrees -> AccidentalFormat
    -> Either Pitch.Degree Pitch.Pitch -> Pitch.Note

sargam :: RelativeFormat key -> Format
sargam = make_relative_format (make_pattern degrees) degrees
    where degrees = make_degrees ["s", "r", "g", "m", "p", "d", "n"]

cipher :: Pitch.PitchClass -> RelativeFormat key -> Format
cipher pc_per_octave = make_relative_format pattern degrees
    where
    degrees = make_degrees ["-" <> showt pc | pc <- [1..pc_per_octave]]
    pattern = "-[1-" <> showt pc_per_octave <> "]"

zh_cipher :: Pitch.PitchClass -> RelativeFormat key -> Format
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

-- | This is the central data structure for this module.  It has the set of
-- functions needed to parse and generate symbolic pitches.  Making one of
-- these gives you access to the set of functions in here, such as 'show_pitch'
-- and 'read_pitch', which in turn can be used to implement
-- a 'Derive.Scale.Scale'.
data Format = Format {
    fmt_show :: ShowPitch
    -- | This doesn't need the key because that work is split off to
    -- 'fmt_to_absolute'.
    , fmt_read :: A.Parser RelativePitch
    -- | I need the key to parse the pitch, but that can't happen unless I want
    -- to give all pattern_lookup calls access to the env in scope.  But
    -- I don't need the env to recognize if it's a valid call or not.
    , fmt_to_absolute :: Maybe Pitch.Key -> RelativePitch
        -> Either DeriveT.PitchError Pitch.Pitch
    -- | Goes in 'Scale.scale_pattern'.
    , fmt_pattern :: !Text
    -- TODO why do I need this?  can I have keys with different octaves?
    -- It's used only by JustScales
    , fmt_pc_per_octave :: Pitch.PitchClass
    -- | True if this scale is relative to the key.  Only used by
    -- ChromaticScales.  TODO split it out?
    , fmt_relative :: !Bool
    }

-- | This is used to show both a Pitch with an octave, and a Degree without
-- one (used for key names).  This is because the code is presumably mostly the
-- same.
type ShowPitch = Maybe Pitch.Key -> Either Pitch.Degree Pitch.Pitch
    -> Pitch.Note
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
    intervals = commas $ map showt $ Vector.toList $ Theory.key_intervals key
    commas = Text.intercalate ", "

-- | Show the signature of the given key by showing each scale degree with the
-- the accidentals implied by the key signature.
show_signature :: Format -> Theory.Key -> [Text]
show_signature fmt key =
    map (show_degree fmt Nothing . Pitch.pitch_degree) pitches
    where
    pc_per_octave = Vector.length (Theory.key_intervals key)
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
    -> Either DeriveT.PitchError Pitch.Note
scale_show_pitch fmt key = Right . show_pitch fmt key

show_degree :: Format -> Maybe Pitch.Key -> Pitch.Degree -> Text
show_degree fmt key = Pitch.note_text . fmt_show fmt key . Left

read_pitch :: Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
read_pitch fmt key = fmt_to_absolute fmt key <=< read_relative_pitch fmt

-- | Parse a Note, but don't adjust it for the key.  This means that relative
-- pitches will likely be incorrect.  'ToAbsolute' documents why this needs
-- to be separate.
read_relative_pitch :: Format -> Pitch.Note
    -> Either DeriveT.PitchError RelativePitch
read_relative_pitch fmt note = justErr (DeriveT.UnparseableNote note) $
    Debug.trace_ret "read_rel" note $
    ParseText.maybe_parse (fmt_read fmt) $
    Pitch.note_text note

-- ** make

make_absolute_format :: Text -> Degrees -> Format
make_absolute_format = make_absolute_format_config default_config

make_absolute_format_keyed :: Config -> KeyConfig Theory.Key
    -> Text -> Degrees -> Format
make_absolute_format_keyed config key_config pattern degrees =
    (make_absolute_format_config config pattern degrees)
    { fmt_show = show_pitch_keyed_absolute config key_config degrees
    , fmt_read = p_pitch config degrees
    , fmt_to_absolute = \maybe_key pitch -> do
        key <- key_parse key_config maybe_key
        return $ to_abs key pitch
    }
    where
    -- A note with no explicit accidentals gets the accidentals of the key
    -- signature.
    to_abs key (RelativePitch octave pc maybe_acc) = case maybe_acc of
        Nothing -> Pitch.Pitch octave
            (Pitch.Degree pc (Theory.accidentals_at_pc key pc))
        Just acc -> Pitch.Pitch octave (Pitch.Degree pc acc)

-- | A configurable version of 'make_absolute_format'.
make_absolute_format_config :: Config -> Text -> Degrees -> Format
make_absolute_format_config config pattern degrees = Format
    { fmt_show = show_pitch_absolute config degrees
    , fmt_read = p_pitch config degrees
    , fmt_to_absolute = \_ -> Right . relative_to_absolute
    , fmt_pattern = octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = Vector.length degrees
    , fmt_relative = False
    }

-- | Make a Format from a 'RelativeFormat'.
make_relative_format :: Text -> Degrees -> RelativeFormat key -> Format
make_relative_format pattern degrees rel_fmt = Format
    { fmt_show
    , fmt_read = p_pitch rel_config degrees
    , fmt_to_absolute
    , fmt_pattern = octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = Vector.length degrees
    , fmt_relative = True
    }
    where
    RelativeFormat
        { rel_config, rel_key_config, rel_show_degree, rel_to_absolute
        } = rel_fmt
    fmt_show key = rel_show_degree
        (either (const (key_default rel_key_config)) id
            (key_parse rel_key_config key))
        (config_show_octave rel_config) degrees (config_accidental rel_config)
    fmt_to_absolute maybe_key pitch = do
        key <- key_parse rel_key_config maybe_key
        return $ rel_to_absolute key degrees pitch

acc_pattern :: Text
acc_pattern = "(bb|b|n|#|x)?"

octave_pattern :: Text
octave_pattern = "[-1-9]"

-- *** absolute

show_pitch_absolute :: Config -> Degrees -> ShowPitch
show_pitch_absolute config degrees _key pitch =
    Pitch.Note $ case pitch of
        Left (Pitch.Degree pc acc) ->
            degree <> show_accidentals (config_accidental config) acc
            where (_, degree) = degree_at degrees pc
        Right (Pitch.Pitch oct (Pitch.Degree pc acc)) ->
            config_show_octave config (oct + pc_oct) $
                degree <> show_accidentals (config_accidental config) acc
            where (pc_oct, degree) = degree_at degrees pc

-- *** relative

show_degree_chromatic :: ShowDegree Theory.Key
show_degree_chromatic key show_octave degrees acc_fmt degree_pitch =
    Pitch.Note $ case degree_pitch of
        Left _ -> pc_text <> acc_text
        Right (Pitch.Pitch oct _) -> Debug.trace "show_degree_chromatic" $
            show_octave (oct + pc_oct) (pc_text <> acc_text)
    where
    Pitch.Degree pc acc = either id Pitch.pitch_degree degree_pitch
    acc_text = show_accidentals acc_fmt $ acc - Theory.accidentals_at_pc key pc
    (pc_oct, pc_text) =
        show_pc degrees (Pitch.degree_pc (Theory.key_tonic key)) pc

chromatic_to_absolute :: ToAbsolute Theory.Key
chromatic_to_absolute key degrees (RelativePitch octave pc maybe_acc) =
    Debug.trace_ret "ch to abs" (tonic, (pc, maybe_acc)) $
        Pitch.Pitch (octave + oct) (Pitch.Degree pc2 acc2)
    where
    -- sa in D is D, so add 1 PC for the tonic.
    -- If it's ri in c#-maj then it's considered d# or Relative 1 0 -> Pitch 1 1
    -- So, add the key signature.
    --
    -- But for pelog, Relative 3 -> Pitch 2 1.  So I take Relative as
    -- white notes and subtract layout to make Pitch into something relative to
    -- pathet (2+1) but which winds up being absolute "4" if acc + pc.
    -- Isn't it overcomplicated?  Why not Relative 3 -> Pitch 3, then show
    -- Pitch 3 in pelog is "4".  The place to care about pathet is diatonic
    -- transposition and kbd to call.
    (oct, pc2) = (pc + Pitch.degree_pc tonic) `divMod` Vector.length degrees
    acc2 = fromMaybe 0 maybe_acc + case Theory.key_signature key of
        -- If it's chromatic then I can't adjust for the mode, but I still
        -- want to map degree 1 to C# if I'm in C#.
        Nothing -> Pitch.degree_accidentals tonic
        Just sig -> fromMaybe 0 (sig Vector.!? pc) -- Should never get Nothing.
    tonic = Theory.key_tonic key

type Tonic = Pitch.PitchClass

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
show_pc degrees tonic pc = (oct, degrees ! i)
    where (oct, i) = (pc - tonic) `divMod` Vector.length degrees

degree_at :: Degrees -> Pitch.PitchClass -> (Pitch.Octave, Text)
degree_at degrees pc = show_pc degrees 0 pc

-- | Convert a relative pitch using a simple diatonic key system, where the
-- key is just a note in the scale.
diatonic_to_absolute :: ToAbsolute Tonic
diatonic_to_absolute tonic degrees (RelativePitch octave pc maybe_acc) =
    Pitch.Pitch (octave + oct) (Pitch.Degree pc2 (fromMaybe 0 maybe_acc))
    where (oct, pc2) = (pc + tonic) `divMod` Vector.length degrees

-- *** keyed relative

-- | Like 'show_degree_chromatic', but format the accidentals as staff notation
-- does, in that accidentals implicit in the key signature are omitted, and
-- a natural that differs from the key signature is emitted.
-- TODO tons of args, can I package some up?
show_pitch_keyed_absolute :: Config -> KeyConfig Theory.Key -> Degrees
    -> ShowPitch
show_pitch_keyed_absolute config key_config degrees maybe_key degree_pitch =
    Pitch.Note $ case degree_pitch of
        Left _ -> pitch
        Right (Pitch.Pitch oct _) -> show_octave (oct + pc_oct) pitch
    where
    Pitch.Degree pc_ acc = either id Pitch.pitch_degree degree_pitch
    (pc_oct, pc) = pc_ `divMod` Vector.length degrees
    key = either (const (key_default key_config)) id
        (key_parse key_config maybe_key)
    pitch = degrees ! pc <> acc_text
    acc_text = show_accidentals_keyed (config_accidental config)
        (Theory.accidentals_at_pc key pc) acc

-- * parse

p_pitch :: Config -> Degrees -> A.Parser RelativePitch
p_pitch config degrees =
    config_parse_octave config
        ((,) <$> p_degree <*> p_accidentals (config_accidental config))
    where
    p_degree = A.choice
        [ A.string text >> return i
        | (i, text) <- zip [0..] (Vector.toList degrees)
        ]

-- ** octave

-- | Most scales display the octave as a leading number.
show_octave :: ShowOctave
show_octave oct = (showt oct <>)

parse_octave :: ParseOctave
parse_octave parse = do
    oct <- ParseText.p_int
    (pc, acc) <- parse
    return $ RelativePitch oct pc acc

-- | Require a single digit octave.
parse_octave1 :: ParseOctave
parse_octave1 parse = do
    oct <- maybe mzero pure . Num.readDigit =<< A.satisfy ParseText.is_digit
    (pc, acc) <- parse
    return $ RelativePitch oct pc acc

-- ** accidentals

-- | natural, sharp1, sharp2, flat1, flat2
data AccidentalFormat = AccidentalFormat !Text !Text !Text !Text !Text
    deriving (Show)

ascii_accidentals :: AccidentalFormat
ascii_accidentals = AccidentalFormat "n" "#" "x" "b" "bb"

symbol_accidentals :: AccidentalFormat
symbol_accidentals = AccidentalFormat "`n`" "`#`" "`##`" "`b`" "`bb`"

p_accidentals :: AccidentalFormat -> A.Parser (Maybe Pitch.Accidentals)
p_accidentals (AccidentalFormat natural sharp1 sharp2 flat1 flat2) =
    p_natural <|> Just . Num.sum <$> A.many1 p_acc <|> pure Nothing
    where
    p_natural = A.string natural *> return (Just 0)
    p_acc = A.choice
        [ A.string sharp1 *> return 1
        , A.string sharp2 *> return 2
        , A.string flat1 *> return (-1)
        , A.string flat2 *> return (-2)
        ]

-- | Show accidentals relative to the key signature.
show_accidentals_keyed :: AccidentalFormat -> Pitch.Accidentals
    -> Pitch.Accidentals -> Text
show_accidentals_keyed fmt@(AccidentalFormat natural _ _ _ _) key_accs accs
    | accs == key_accs = ""
    | accs == 0 && key_accs /= 0 = natural
    | otherwise = show_accidentals fmt accs

show_accidentals :: AccidentalFormat -> Pitch.Accidentals -> Text
show_accidentals (AccidentalFormat _ sharp1 sharp2 flat1 flat2) acc
    | acc == 0 = ""
    | acc < 0 = Text.replicate x flat2 <> Text.replicate s flat1
    | otherwise = Text.replicate x sharp2 <> Text.replicate s sharp1
    where (x, s) = abs acc `divMod` 2
