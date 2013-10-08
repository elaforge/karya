-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to construct scales.
module Derive.Scale.Util where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Map as Map
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Call as Call
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- | Make a simple scale where there is a direct mapping from input to note to
-- nn.
make_scale :: DegreeMap -> Pitch.ScaleId -> Text -> Text -> Scale.Scale
make_scale dmap scale_id note_pattern doc = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = note_pattern
    , Scale.scale_symbols = []
    , Scale.scale_transposers = standard_transposers
    , Scale.scale_transpose = transpose dmap
    , Scale.scale_enharmonics = no_enharmonics
    , Scale.scale_note_to_call = mapped_note_to_call dmap scale
    , Scale.scale_input_to_note = input_to_note dmap
    , Scale.scale_input_to_nn = mapped_input_to_nn dmap
    , Scale.scale_call_doc = call_doc standard_transposers dmap doc
    }
    where scale = PitchSignal.Scale scale_id standard_transposers

-- * types

data DegreeMap = DegreeMap {
    dm_to_degree :: Map.Map Pitch.Note Pitch.Degree
    , dm_to_note :: Map.Map Pitch.Degree Pitch.Note
    , dm_to_nn :: Map.Map Pitch.Degree Pitch.NoteNumber
    -- | Number of scale steps per octave.  Actually, simple scales are just
    -- a collection of frequencies and don't need to have a notion of an
    -- octave.  But since the input mechanism wants to orient around octaves,
    -- it needs to know how many keys to assign to each octave.  So if your
    -- scale has no octaves, then just set this to 8, that way it lines up with
    -- the piano keyboard.
    , dm_per_octave :: Pitch.Degree
    , dm_pitch_to_degree :: Theory.Pitch -> Pitch.Degree
    }

degree_map :: Pitch.Degree -- ^ see 'dm_per_octave'
    -- | First Note is this octave and PitchClass.
    -> Theory.Octave -> Pitch.Degree
    -> [Pitch.Note] -> [Pitch.NoteNumber] -> DegreeMap
degree_map per_octave start_octave start_degree notes_ nns_ = DegreeMap
    { dm_to_degree = Map.fromList (zip notes [0..])
    , dm_to_note = Map.fromList (zip [0..] notes)
    , dm_to_nn = Map.fromList (zip [0..] nns)
    , dm_per_octave = per_octave
    , dm_pitch_to_degree = pitch_to_degree per_octave start_octave start_degree
    }
    where
    -- Guard against an infinite notes or nns.
    (notes, nns) = unzip $ zip notes_ nns_

pitch_to_degree :: Pitch.Degree -> Pitch.Octave -> Pitch.Degree -> Theory.Pitch
    -> Pitch.Degree
pitch_to_degree (Pitch.Degree per_octave) start_octave start
        (Theory.Pitch octave (Theory.Note pc accs)) =
    Pitch.Degree ((octave - start_octave) * per_octave + pc + accs) - start

type DegreeToNoteNumber = PitchSignal.PitchConfig -> Pitch.Degree
    -> Either Scale.ScaleError Pitch.NoteNumber

-- * scale functions

-- ** transpose

transpose :: DegreeMap -> Derive.Transpose
transpose dmap = \_key octaves steps note -> do
    note_degree <- maybe (Left Scale.UnparseableNote) Right
        (Map.lookup note (dm_to_degree dmap))
    let degrees = case steps of
            Pitch.Diatonic steps -> d (floor steps)
            Pitch.Chromatic steps -> d (floor steps)
    maybe (Left Scale.InvalidTransposition) Right $
        Map.lookup
            (note_degree + d octaves * dm_per_octave dmap + degrees)
            (dm_to_note dmap)
    where d = Pitch.Degree

-- | Transpose function for a non-transposing scale.
non_transposing :: Derive.Transpose
non_transposing _ _ _ _ = Left Scale.InvalidTransposition

standard_transposers :: Set.Set Score.Control
standard_transposers = Set.fromList
    [Controls.chromatic, Controls.diatonic, Controls.nn, Controls.hz]

-- ** note_to_call

-- | A specialization of 'note_to_call' that operates on scales with
-- a ScaleMap, i.e. a static map from notes to degrees, and from degrees to
-- NNs.
mapped_note_to_call :: DegreeMap -> PitchSignal.Scale
    -> Pitch.Note -> Maybe Derive.ValCall
mapped_note_to_call dmap scale = note_to_call scale dmap to_nn
    where
    to_nn _config degree =
        maybe (Left Scale.InvalidTransposition) Right $
            Map.lookup degree (dm_to_nn dmap)

-- | Create a note call that respects chromatic and diatonic transposition.
-- However, diatonic transposition is mapped to chromatic transposition,
-- so this is for scales that don't distinguish.
note_to_call :: PitchSignal.Scale -> DegreeMap -> DegreeToNoteNumber
    -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale dmap degree_to_nn note =
    case Map.lookup note (dm_to_degree dmap) of
        Nothing -> Nothing
        Just degree -> Just $ ScaleDegree.scale_degree scale
            (pitch_nn degree) (pitch_note degree)
    where
    pitch_nn :: Pitch.Degree -> Scale.PitchNn
    pitch_nn (Pitch.Degree degree) config =
        scale_to_pitch_error diatonic chromatic $
            to_note (fromIntegral degree + chromatic + diatonic) config
        where
        controls = PitchSignal.pitch_controls config
        chromatic = Map.findWithDefault 0 Controls.chromatic controls
        diatonic = Map.findWithDefault 0 Controls.diatonic controls
    to_note degree config
        | frac == 0 = to_nn int
        | otherwise = Num.scale <$> to_nn int <*> to_nn (int+1)
            <*> return (Pitch.NoteNumber frac)
        where
        (int, frac) = properFraction degree
        to_nn = degree_to_nn config . Pitch.Degree . fromIntegral
    pitch_note :: Pitch.Degree -> Scale.PitchNote
    pitch_note (Pitch.Degree degree) config =
        maybe (Left err) Right $ Map.lookup transposed (dm_to_note dmap)
        where
        err = invalid_transposition diatonic chromatic
        transposed = Pitch.Degree $ round $
            fromIntegral degree + chromatic + diatonic
        chromatic = Map.findWithDefault 0 Controls.chromatic controls
        diatonic = Map.findWithDefault 0 Controls.diatonic controls
        controls = PitchSignal.pitch_controls config

lookup_key :: TrackLang.Environ -> Maybe Pitch.Key
lookup_key = fmap Pitch.Key . TrackLang.maybe_val Environ.key

scale_to_pitch_error :: Signal.Y -> Signal.Y
    -> Either Scale.ScaleError a -> Either PitchSignal.PitchError a
scale_to_pitch_error diatonic chromatic = either (Left . msg) Right
    where
    msg err = case err of
        Scale.InvalidTransposition -> invalid_transposition diatonic chromatic
        Scale.KeyNeeded -> PitchSignal.PitchError
            "no key is set, but this transposition needs one"
        Scale.UnparseableEnviron name val -> PitchSignal.PitchError $
            txt (Pretty.pretty name) <> " unparseable by given scale: " <> val
        Scale.UnparseableNote -> PitchSignal.PitchError
            "unparseable note (shouldn't happen)"

invalid_transposition :: Signal.Y -> Signal.Y -> PitchSignal.PitchError
invalid_transposition diatonic chromatic =
    PitchSignal.PitchError $ "note can't be transposed: "
        <> Text.unwords (filter (not . Text.null)
            [fmt "d" diatonic, fmt "c" chromatic])
    where
    fmt _ 0 = ""
    fmt code val = txt (Pretty.pretty val) <> code

-- ** input

type InputToNote = Maybe Pitch.Key -> Pitch.Input -> Maybe Pitch.Note

-- | Input to note for simple scales without keys.
input_to_note :: DegreeMap -> InputToNote
input_to_note dmap _key (Pitch.Input kbd pitch frac) = do
    pitch <- simple_kbd_to_scale dmap kbd pitch
    let degree = dm_pitch_to_degree dmap pitch
    note <- Map.lookup degree (dm_to_note dmap)
    return $ ScaleDegree.pitch_expr frac note

-- | Input to NoteNumber for scales that have a direct relationship between
-- Degree and NoteNumber.
mapped_input_to_nn :: DegreeMap
    -> (ScoreTime -> Pitch.Input -> Derive.Deriver (Maybe Pitch.NoteNumber))
mapped_input_to_nn dmap =
    \_pos (Pitch.Input kbd pitch frac) -> return $ do
        pitch <- simple_kbd_to_scale dmap kbd pitch
        to_nn (dm_pitch_to_degree dmap pitch) frac
    where
    to_nn degree frac
        | frac == 0 = lookup degree
        | frac > 0 = do
            nn <- lookup degree
            next <- lookup (degree + 1)
            return $ Num.scale nn next (Pitch.NoteNumber frac)
        | otherwise = do
            nn <- lookup degree
            prev <- lookup (degree - 1)
            return $ Num.scale prev nn (Pitch.NoteNumber (frac + 1))
    lookup d = Map.lookup d (dm_to_nn dmap)

-- | An Input maps directly to a NoteNumber.  This is an efficient
-- implementation for scales tuned to 12TET.
direct_input_to_nn :: ScoreTime -> Pitch.Input
    -> Derive.Deriver (Maybe Pitch.NoteNumber)
direct_input_to_nn _pos (Pitch.Input _ pitch frac) =
    return $ Just $ nn + Pitch.nn frac
    where
    nn = fromIntegral $ Theory.semis_to_nn $
        Theory.pitch_to_semis Theory.piano_layout pitch

-- | Convert input to nn by going through note_to_call.  This works for
-- complicated scales that retune based on the environment but is more work.
computed_input_to_nn :: InputToNote -> (Pitch.Note -> Maybe Derive.ValCall)
    -> ScoreTime -> Pitch.Input -> Derive.Deriver (Maybe Pitch.NoteNumber)
computed_input_to_nn input_to_note note_to_call pos input
    | Just note <- input_to_note Nothing input, Just call <- note_to_call note =
        Call.apply_pitch pos call >>= \val -> case val of
            TrackLang.VPitch pitch -> do
                controls <- Derive.controls_at =<< Derive.real pos
                environ <- Internal.get_environ
                return $ either (const Nothing) Just $
                    PitchSignal.eval_pitch pitch
                        (PitchSignal.PitchConfig environ controls)
            _ -> return Nothing
    | otherwise = return Nothing

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Pitch.Frac -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high (Pitch.NoteNumber frac)

-- *** diatonic

simple_kbd_to_scale :: DegreeMap -> Pitch.KbdType -> Theory.Pitch
    -> Maybe Theory.Pitch
simple_kbd_to_scale dmap kbd =
    kbd_to_scale kbd (fromIntegral (dm_per_octave dmap)) 0

-- | Convert an absolute Pitch in the input keyboard's layout to a relative
-- Pitch within a scale with the given number of diatonic steps per octave, or
-- Nothing if that key should have no pitch.
kbd_to_scale :: Pitch.KbdType -> Theory.PitchClass -> Theory.PitchClass
    -> Theory.Pitch -> Maybe Theory.Pitch
kbd_to_scale kbd pc_per_octave tonic pitch = case kbd of
    Pitch.PianoKbd -> piano_kbd_pitch tonic pc_per_octave pitch
    Pitch.AsciiKbd -> Just $ ascii_kbd_pitch pc_per_octave pitch

-- | Scale octave doesn't match the kbd octave, but is absolute:
--
--    C D E F G A B|C D E F G A B
-- C  1 2 3 4 5 - - 1 2 3 4 5 - -
-- D  - 1 2 3 4 5 - - 1 2 3 4 5 -
-- E  - - 1 2 3 4 5 - - 1 2 3 4 5
--
-- Piano:
--    0 1 2 3 4 5 6 0 1 2 3 4 5 6 0
--    C D E F G A B|C D E F G A B|C
--    0 1 2 3 4 - - 0
--    0 1 2 3 4 5 6 7 8 - - - - - 0

-- | The MIDI kbd is absolute.  This means that relative scales start on
-- different keys rather than all starting on C.  For example, in C major
-- C produces the first scale degree, while in D major D produces the first
-- scale degree.
--
-- In addition, if the scale octave is not an even multiple of the kbd octave
-- (7), the extra notes produce Nothing.  This check has to be done to
-- the relative PitchClass.  That way, a D on a 6 note scale starting on D is
-- 1, and a C is Nothing.  Thus, the returned Pitch is relative to the given
-- tonic, so it should be formatted as-is, without the key.
piano_kbd_pitch :: Theory.PitchClass -> Theory.PitchClass -> Theory.Pitch
    -> Maybe Theory.Pitch
piano_kbd_pitch tonic pc_per_octave (Theory.Pitch oct (Theory.Note pc accs))
    | relative_pc >= pc_per_octave = Nothing
    | otherwise =
        Just $ Theory.Pitch (oct1 + oct_diff) (Theory.Note relative_pc accs)
    where
    (oct1, pc1) = adjust_octave pc_per_octave 7 oct pc
    (oct_diff, relative_pc) = (pc1 - tonic) `divMod` max_pc
    max_pc = ceiling (fromIntegral pc_per_octave / 7) * 7

-- | The ASCII kbd is relative.  This means that relative scales always start
-- on \"C\".  So the tonic note of a key in a relative scale is irrelevant,
-- C major and D major both start in the same place.  Of course, they produce
-- different frequencies, but that's the responsibility of
-- 'Scale.scale_note_to_call'.
--
-- Unlike 'absolute_to_pitch', if the scale octave is not an even multiple of
-- the kbd octave (10), the extra notes wrap to the next highest octave.
ascii_kbd_pitch :: Theory.PitchClass -> Theory.Pitch -> Theory.Pitch
ascii_kbd_pitch pc_per_octave (Theory.Pitch oct (Theory.Note pc accs)) =
    Theory.Pitch (add_oct + oct1) (Theory.Note pc2 accs)
    where
    (oct1, pc1) = adjust_octave pc_per_octave 10 oct pc
    -- If the scale is shorter than the kbd, go up to the next octave on
    -- the same row.
    (add_oct, pc2) = pc1 `divMod` pc_per_octave

-- | Try to fit a note from a keyboard into a scale.  Round the note up to the
-- nearest multiple of the keyboard octave and adjust the octave accordingly.
adjust_octave :: Theory.PitchClass -> Theory.PitchClass -> Pitch.Octave
    -> Theory.PitchClass -> (Pitch.Octave, Theory.PitchClass)
adjust_octave pc_per_octave kbd_per_octave oct pc =
    (oct2, offset * kbd_per_octave + pc)
    where
    rows = ceiling $ fromIntegral pc_per_octave / fromIntegral kbd_per_octave
    (oct2, offset) = oct `divMod` rows


-- ** call_doc

call_doc :: Set.Set Score.Control -> DegreeMap -> Text -> Derive.DocumentedCall
call_doc transposers dmap doc =
    annotate_call_doc transposers doc fields default_scale_degree_doc
    where
    fields = [("note range", map_range snd (dm_to_note dmap))]
    map_range extract fm = case (Map.min fm, Map.max fm) of
        (Just kv1, Just kv2) -> txt (Pretty.pretty (extract kv1))
            <> " to " <> txt (Pretty.pretty (extract kv2))
        _ -> ""

-- | Documentation of the standard 'Call.Pitch.scale_degree'.
default_scale_degree_doc :: Derive.DocumentedCall
default_scale_degree_doc = scale_degree_doc ScaleDegree.scale_degree

scale_degree_doc ::
    (PitchSignal.Scale -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall)
    -> Derive.DocumentedCall
scale_degree_doc scale_degree =
    Derive.extract_val_doc $ scale_degree PitchSignal.no_scale err err
    where err _ = Left $ PitchSignal.PitchError "it was just an example!"

annotate_call_doc :: Set.Set Score.Control -> Text -> [(Text, Text)]
    -> Derive.DocumentedCall -> Derive.DocumentedCall
annotate_call_doc transposers doc fields = Derive.prepend_doc extra_doc
    where
    extra_doc = doc <> "\n\n" <> join (transposers_field <> fields)
    transposers_field =
        [("transposers", txt $ Pretty.pretty transposers)
            | not (Set.null transposers)]
    join = Text.unlines
        . map (\(k, v) -> k <> ": " <> v) . filter (not . Text.null . snd)

add_doc :: Text -> Scale.Scale -> Scale.Scale
add_doc doc scale = scale
    { Scale.scale_call_doc = Derive.prepend_doc doc (Scale.scale_call_doc scale)
    }

-- * util

no_enharmonics :: Derive.Enharmonics
no_enharmonics _ _ = Right []

read_environ :: (TrackLang.Typecheck a) => (a -> Maybe val) -> val
    -> TrackLang.ValName -> TrackLang.Environ -> Either Scale.ScaleError val
read_environ read_val deflt name env = case TrackLang.get_val name env of
    Left (TrackLang.WrongType expected) ->
        unparseable ("expected type " <> txt (Pretty.pretty expected))
    Left TrackLang.NotFound -> Right deflt
    Right val -> parse val
    where
    parse val = maybe (unparseable (ShowVal.show_val val)) Right (read_val val)
    unparseable = Left . Scale.UnparseableEnviron name

maybe_key :: Pitch.Key -> Maybe a -> Either Scale.ScaleError a
maybe_key (Pitch.Key txt) =
    maybe (Left $ Scale.UnparseableEnviron Environ.key txt) Right
