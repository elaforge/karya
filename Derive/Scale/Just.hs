-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A version of a just intonation 12 note scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Args as Args
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ make_scale (Pitch.ScaleId "just-maj") major_ratios absolute_format
    , make_scale (Pitch.ScaleId "just-min") minor_ratios absolute_format
    , make_scale (Pitch.ScaleId "just-maj-r") major_ratios relative_format
    , make_scale (Pitch.ScaleId "just-min-r") minor_ratios relative_format
    ]

absolute_format :: TheoryFormat.Format
absolute_format = TheoryFormat.absolute_c

relative_format :: TheoryFormat.Format
relative_format =
    TheoryFormat.sargam (fmap key_tonic . lookup_key) 0
        TheoryFormat.show_note_diatonic TheoryFormat.adjust_diatonic

lookup_key :: Maybe Pitch.Key -> Either Scale.ScaleError Key
lookup_key Nothing = Right default_key
lookup_key (Just (Pitch.Key key)) =
    maybe (Left $ Scale.UnparseableEnviron Environ.key key) Right $
        Map.lookup key all_keys

-- | Each accidental adds or subtracts this interval.
accidental_interval :: Double
accidental_interval = 16 / 15

make_scale :: Pitch.ScaleId -> Vector.Vector Ratio.Rational
    -> TheoryFormat.Format -> Scale.Scale
make_scale scale_id ratios fmt = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-g][#b]?"
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose fmt
    , Scale.scale_enharmonics = enharmonics fmt
    , Scale.scale_note_to_call = note_to_call fmt double_ratios
    , Scale.scale_input_to_note = input_to_note (TheoryFormat.show_pitch fmt)
    , Scale.scale_input_to_nn =
        Util.computed_input_to_nn
            (input_to_note (TheoryFormat.show_pitch fmt))
            (note_to_call fmt double_ratios)
    , Scale.scale_call_doc = Util.annotate_call_doc Util.standard_transposers
        ("Just scales are tuned by ratios from a base frequency.\
        \ That frequency is taken from the `%just-base` control and the key.\
        \ For example, `%just-base = 440 | key = a` means that A in the\
        \ middle octave is 440hz and is considered 1/1. If the base hz isn't\
        \ given, it defaults to the 12TET tuning of the key."
        ) [("ratios", Pretty.prettytxt ratios)]
            (Util.scale_degree_doc (scale_degree 0))
    }
    where double_ratios = Vector.map realToFrac ratios

read_note :: TheoryFormat.Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Theory.Pitch
read_note fmt key =
    TheoryFormat.fmt_adjust fmt key <=< TheoryFormat.read_pitch fmt


-- * input_to_note

enharmonics :: TheoryFormat.Format -> Derive.Enharmonics
enharmonics fmt key note = do
    pitch <- read_note fmt key note
    return $ map (TheoryFormat.show_pitch fmt key) $
        Theory.enharmonics_of layout pitch

input_to_note :: TheoryFormat.ShowPitch -> Maybe Pitch.Key -> Pitch.InputKey
    -> Maybe Pitch.Note
input_to_note show_pitch _key (Pitch.InputKey nn) =
    -- I ignore the key on input.  Otherwise, a InputKey on middle C will be
    -- adjusted for the key, but I want middle C to be the sa of a relative
    -- scale.
    Just $ show_pitch Nothing $ Theory.semis_to_pitch_sharps layout $
        Theory.nn_to_semis $ floor nn

layout :: Theory.Layout
layout = Theory.layout TheoryFormat.absolute_c_intervals

-- | Since just scales don't really have key signatures or even accidentals,
-- this only even produces sharps.
degree_note :: TheoryFormat.ShowPitch -> Maybe Pitch.Key -> Double -> Pitch.Note
degree_note show_pitch key degreef =
    Pitch.Note $ Call.Pitch.pitch_expr note frac
    where
    (degree, frac) = properFraction degreef
    (octave, pc) = degree `divMod` pc_per_octave
    note = show_pitch key $ Theory.Pitch octave (Theory.Note pc 0)

-- | Unused, but still here in case I don't like accidentals.
input_to_note_no_acc :: TheoryFormat.ShowPitch -> Maybe Pitch.Key
    -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note_no_acc show_pitch key (Pitch.InputKey nn) =
    case (degree1, degree2) of
        (Just d1, Just d2) ->
            Just $ degree_note show_pitch key (Num.scale d1 d2 frac)
        _ -> Nothing
    where
    degree1 = nn_to_degree Vector.!? input_degree
    degree2 = nn_to_degree Vector.!? (input_degree + 1)
    (input_degree, frac) = properFraction nn

nn_to_degree :: Vector.Vector Double
nn_to_degree = Vector.fromList $ take 127 $
    to_steps (zip [0..] (cycle TheoryFormat.absolute_c_intervals))
    where
    to_steps [] = []
    to_steps ((n, step) : steps)
        | step == 2 = n : n + 0.5 : to_steps steps
        | otherwise = n : to_steps steps


-- * transpose

transpose :: TheoryFormat.Format -> Maybe Pitch.Key -> Pitch.Octave
    -> Pitch.Transpose -> Pitch.Note -> Either Scale.ScaleError Pitch.Note
transpose fmt key oct transpose note = do
    pitch <- read_note fmt key note
    let steps = floor $ case transpose of
            Pitch.Chromatic steps -> steps
            Pitch.Diatonic steps -> steps
    Right $ TheoryFormat.show_pitch fmt key $
        Theory.transpose_pitch pc_per_octave (oct * pc_per_octave + steps) pitch

pc_per_octave :: Theory.PitchClass
pc_per_octave = 7

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
note_to_call :: TheoryFormat.Format -> Ratios -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call fmt ratios note = case TheoryFormat.read_pitch fmt note of
    Left _ -> case parse_relative_interval note of
        Nothing -> Nothing
        Just interval -> Just $ relative_scale_degree interval
    Right pitch_ ->
        let pitch = pitch_ { Theory.pitch_note = (Theory.pitch_note pitch_)
                { Theory.note_accidentals = 0 } }
            accs = Theory.pitch_accidentals pitch_
        in Just $ scale_degree accs (pitch_nn pitch) (pitch_note pitch)
    where
    pitch_nn :: Theory.Pitch -> Scale.PitchNn
    pitch_nn pitch env controls =
        Util.scale_to_pitch_error chromatic diatonic $ do
            key <- read_key env
            pitch <- TheoryFormat.fmt_adjust fmt (Util.lookup_key env) pitch
            let hz = transpose_to_hz ratios base_hz key
                    (chromatic + diatonic) pitch
                nn = Pitch.hz_to_nn hz
            if Num.in_range 0 127 nn then Right nn
                else Left Scale.InvalidTransposition
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls
        base_hz = Map.lookup just_base_control controls

    pitch_note :: Theory.Pitch -> Scale.PitchNote
    pitch_note pitch env controls =
        Util.scale_to_pitch_error chromatic diatonic $ do
            let key = Util.lookup_key env
            pitch <- TheoryFormat.fmt_adjust fmt key pitch
            let transposed = Theory.transpose_pitch pc_per_octave
                    (round (chromatic + diatonic)) pitch
            Right $ TheoryFormat.show_pitch fmt key transposed
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls

just_base_control :: Score.Control
just_base_control = Score.Control "just-base"

scale_degree :: Theory.Accidentals -> Scale.PitchNn -> Scale.PitchNote
    -> Derive.ValCall
scale_degree accs pitch_nn pitch_note = Derive.val_call "pitch" Tags.scale
    "Emit the pitch of a scale degree." $ Sig.call intervals_arg $
    \intervals _ -> do
        let acc_interval = accidental_interval ^^ accs
        interval <- resolve_intervals intervals
        environ <- Internal.get_dynamic Derive.state_environ
        return $! TrackLang.VPitch $ PitchSignal.pitch
            (call (acc_interval * interval) environ) (pitch_note environ)
    where
    call interval environ controls =
        modify interval (Call.Pitch.get_hz controls) <$>
            pitch_nn environ controls
    modify interval hz
        | interval == 1 && hz == 0 = id
        | otherwise = Pitch.modify_hz ((+hz) . (*interval))

intervals_arg :: Sig.Parser [Either Pitch.Hz Text]
intervals_arg = Sig.many "interval" $
    "Multiply this interval with the note's frequency. Negative numbers\
    \ divide, so while `3/2` goes up a fifth, `-3/2` goes down a fifth.\
    \ Can be either a ratio or a symbol drawn from: "
    <> Text.intercalate ", " (Map.keys named_intervals)

resolve_intervals :: [Either Pitch.Hz Text] -> Derive.Deriver Pitch.Hz
resolve_intervals intervals =
    product . map unsign <$> mapM (either return resolve) intervals
    where
    resolve text = Derive.require ("unknown named interval: " <> show text) $
        resolve_interval text
    unsign val = if val < 0 then recip (abs val) else val

resolve_interval :: Text -> Maybe Pitch.Hz
resolve_interval text = case Text.uncons text of
    Just ('-', text) -> negate <$> lookup text
    _ -> lookup text
    where lookup text = realToFrac <$> Map.lookup text named_intervals

-- ** relative interval

parse_relative_interval :: Pitch.Note -> Maybe Pitch.Hz
parse_relative_interval note =
    unsign <$> (resolve_interval (Pitch.note_text note) `mplus` parse_num)
    where
    parse_num = case ParseBs.parse_val (Pitch.note_text note) of
        Right (TrackLang.VNum (Score.Typed Score.Untyped num)) -> Just num
        _ -> Nothing
    unsign val = if val < 0 then recip (abs val) else val

relative_scale_degree :: Pitch.Hz -> Derive.ValCall
relative_scale_degree initial_interval =
    Derive.val_call "pitch" Tags.scale "doc doc" $ Sig.call intervals_arg $
    \intervals args -> do
        interval <- (initial_interval*) <$> resolve_intervals intervals
        case Args.prev_val args of
            Just (_, Derive.TagPitch prev) ->
                return $ TrackLang.VPitch (modify interval prev)
            _ -> Derive.throw "relative interval requires a previous pitch"
    where
    modify interval pitch =
        PitchSignal.pitch (pitch_nn interval pitch)
            (PitchSignal.eval_note pitch)
    pitch_nn interval pitch controls = do
        nn <- PitchSignal.eval_pitch pitch controls
        return $ Pitch.modify_hz (*interval) nn

-- ** implementation

transpose_to_hz :: Ratios -> Maybe Pitch.Hz -> Key -> Double
    -> Theory.Pitch -> Pitch.Hz
transpose_to_hz ratios base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    (steps, frac) = properFraction frac_steps
    pitch1 = Theory.transpose_pitch pc_per_octave steps pitch
    hz1 = degree_to_hz pc_per_octave ratios base_hz key pitch1
    hz2 = degree_to_hz pc_per_octave ratios base_hz key
        (Theory.transpose_pitch pc_per_octave 1 pitch1)

-- | Given a Key, convert a pitch in that key to its hz value.
-- If the base hz is given, this is the frequency of the key's tonic.
-- Otherwise, the base is taken from the 12TET value.
degree_to_hz :: Theory.Degree -> Ratios -> Maybe Pitch.Hz
    -> Key -> Theory.Pitch -> Pitch.Hz
degree_to_hz per_oct ratios maybe_base_hz key pitch = base * ratio
    where
    base = base_hz * 2 ^^ Theory.pitch_octave degree
    base_hz = Pitch.nn_to_hz $ normalize_octave $
        maybe (key_tonic_nn key) Pitch.hz_to_nn maybe_base_hz
    ratio = index_mod ratios (Theory.note_pc (Theory.pitch_note degree))
    degree = Theory.transpose_pitch per_oct (- key_tonic key) pitch

-- | Normalize a NoteNumber to lie within the first 'Theory.Pitch' octave.
normalize_octave :: Pitch.NoteNumber -> Pitch.NoteNumber
normalize_octave = (`Num.fmod` 12)

-- * key

data Key = Key {
    -- | PitchClass starts at A, not C.
    key_tonic :: !Theory.PitchClass
    -- | NoteNumber in the bottom octave as returned by 'normalize_octave',
    -- e.g. -3--9.
    , key_tonic_nn :: !Pitch.NoteNumber
    } deriving (Show)

all_keys :: Map.Map Text Key
all_keys =
    Map.fromList $ take 7 [(d, Key pc nn)
        | (d, pc, nn) <- zip3 TheoryFormat.absolute_c_degrees [0..] nns]
    where nns = scanl (+) 0 (cycle [2, 2, 1, 2, 2, 2, 1])

default_key :: Key
Just default_key = Map.lookup "c" all_keys

read_key :: TrackLang.Environ -> Either Scale.ScaleError Key
read_key = Util.read_environ (\k -> Map.lookup k all_keys)
    default_key Environ.key

-- * ratios

index_mod :: Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

type Ratios = Vector.Vector Double

-- | 5-limit diatonic, with just major triads.
major_ratios :: Vector.Vector Ratio.Rational
major_ratios = Vector.fromList [1, 9%8, 5%4, 4%3, 3%2, 5%3, 15%8]

-- | 5-limit diatonic, with just minor triads.
minor_ratios :: Vector.Vector Ratio.Rational
minor_ratios = Vector.fromList [1, 9%8, 6%5, 4%3, 3%2, 8%5, 9%5]

named_intervals :: Map.Map Text Ratio.Rational
named_intervals = Map.fromList
    [ ("m2-", 25 % 24) -- 71, 5-limit minor half-step
    , ("m2", 16 % 15) -- 112, 5-limit major half-step
    , ("M2-", 10 % 9) -- 182, minor whole tone
    , ("M2", 9 % 8) -- 204, 5-limit major second
    , ("m3", 6 % 5) -- 316, 5-limit minor third
    , ("M3", 5 % 4) -- 386, 5-limit major third
    , ("P4", 4 % 3) -- 498, perfect fourth
    , ("tt11", 11 % 8) -- 551, undecimal tritone
    , ("tt7-", 7 % 5) -- 583, septimal tritone
    , ("tt", 45 % 32) -- 590, high 5-limit tritone
    , ("tt7+", 10 % 7) -- 618, septimal tritone
    , ("wolf", 40 % 27) -- 681, wolf 5-limit 5th
    , ("P5", 3 % 2) -- 702, perfect fifth
    , ("m6", 8 % 5) -- 5-limit minor sixth
    , ("M6", 5 % 3) -- 5-limit major sixth
    , ("m7", 9 % 5) -- 5-limit large minor seventh
    , ("M7", 15 % 8) -- 5-limit major seventh
    ]


{- Retuning scales:

    build :: Ratios -> Pitch.Degree -> Pitch.Degree -> Pitch.Hz -> [Pitch.Hz]
    build ratios per_oct degree base =
        List.sort $ map calc (zip [degree..] (Vector.toList ratios))
        where
        calc (d, rat)
            | d >= per_oct = f / 2
            | otherwise = f
            where f = base * realToFrac rat

    interpolated ratios per_oct degree from_base to_base frac =
        where
        v1 = build ratios per_oct degree from_base
        v2 = build ratios per_oct degree to_base

             A       B       C       D       E       F       G       A
    A440     440     495     550     586.6   660     773.3   825     880
    C550     458.3   515.625 550     618.75  687.5   733.3   825     916.6
    A458.3   458.3   515.587 527.875 611.06  687.45  763.83  859.312 970.6

    So if I'm going A->C I can't go through B.  I have to figure out the
    frequencies for C550 and then interpolate to those.

    The tuning pitch must be [(C, A, 0.5)].  So it starts A440, then has C.
    C maps to ratio scale of 5:4, so we set
    (-base = 440, -from = 'a', -to = 550, %-frac = %)
    (-base = 550, -from = 'c', -to = 458.3, %-frac = %)

    What I really need is base_from and base_to, where base is always A.  So
    if the tuning pitch says C then I have to figure out the A of C, based on
    the previous frequency.  I think I can do this if

    But how could this work if pitches are moving up and down?

    f0 = build major_ratios 7
    b1 = f0 0 440
    b2 = f0 2 550
    b3 = f0 0 (458 + 1/3)
-}
