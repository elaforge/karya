-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for simple scales, which simply map pitch names to frequencies.
-- Ok, so they also have octave structure, used by the input mechanism and to
-- parse to 'Pitch.Pitch'es, but it can be set to the number of degrees in the
-- scale if you don't have octaves.
module Derive.Scale.Scales where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector ((!?))

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.TextUtil as TextUtil

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import Global
import Types


-- | Make a simple scale where there is a direct mapping from input to note to
-- nn.
make_scale :: Pitch.ScaleId -> DegreeMap -> Text -> Doc.Doc -> Scale.Scale
make_scale scale_id dmap pattern doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = pattern
    , scale_symbols = []
    , scale_transposers = standard_transposers
    , scale_read = const $ read_note dmap
    , scale_show = const $ show_pitch dmap
    , scale_bottom = Pitch.pitch (dm_start_octave dmap) (dm_start_pc dmap)
    , scale_layout = Scale.diatonic_layout (dm_per_octave dmap)
    , scale_transpose = transpose dmap
    , scale_enharmonics = no_enharmonics
    , scale_note_to_call = mapped_note_to_call dmap scale
    , scale_input_to_note = input_to_note dmap
    , scale_input_to_nn = mapped_input_to_nn dmap
    , scale_call_doc = call_doc standard_transposers dmap doc
    }
    where scale = PSignal.Scale scale_id standard_transposers

-- | An empty scale that doesn't do anything.
empty_scale :: Pitch.ScaleId -> Text -> Derive.DocumentedCall -> Scale.Scale
empty_scale scale_id pattern doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = pattern
    , scale_symbols = []
    , scale_transposers = standard_transposers
    , scale_read = \_ _ -> Left BaseTypes.NotImplemented
    , scale_show = \_ _ -> Left BaseTypes.NotImplemented
    , scale_bottom = Pitch.pitch 1 0
    , scale_layout = Scale.layout []
    , scale_transpose = \_ _ _ _ -> Left BaseTypes.NotImplemented
    , scale_enharmonics = no_enharmonics
    , scale_note_to_call = const Nothing
    , scale_input_to_note = \_ _ -> Left BaseTypes.NotImplemented
    , scale_input_to_nn = \ _ _ -> return $ Left BaseTypes.NotImplemented
    , scale_call_doc = doc
    }

-- * types

data DegreeMap = DegreeMap {
    dm_to_semis :: Map.Map Pitch.Note Pitch.Semi
    , dm_to_note :: Vector.Vector Pitch.Note
    , dm_to_nn :: Vector.Vector Pitch.NoteNumber
    -- | Number of scale steps per octave.  Actually, simple scales are just
    -- a collection of frequencies and don't need to have a notion of an
    -- octave.  But since the input mechanism wants to orient around octaves,
    -- it needs to know how many keys to assign to each octave.  So if your
    -- scale has no octaves, then just set this to 7, that way it lines up with
    -- the piano keyboard.
    , dm_per_octave :: Pitch.Semi
    , dm_start_octave :: Pitch.Octave
    , dm_start_pc :: Pitch.PitchClass
    } deriving (Show)

instance Pretty.Pretty DegreeMap where
    format dmap = Pretty.format $ Map.fromList $ do
        (note, semis) <- Map.toList (dm_to_semis dmap)
        return (semis, (note, dm_to_nn dmap !? semis))

degree_map :: Pitch.PitchClass
    -> Pitch.Octave -- ^ The first Note is this Octave and PitchClass.
    -> Pitch.PitchClass
    -> [Pitch.Note] -> [Pitch.NoteNumber] -> DegreeMap
degree_map per_octave start_octave start_pc notes_ nns_ = DegreeMap
    { dm_to_semis = Map.fromList (zip notes [0..])
    , dm_to_note = Vector.fromList notes
    , dm_to_nn = Vector.fromList nns
    , dm_per_octave = per_octave
    , dm_start_octave = start_octave
    , dm_start_pc = start_pc
    }
    where
    -- Guard against infinite notes or nns.
    (notes, nns) = unzip $ zip notes_ nns_

type SemisToNoteNumber = PSignal.PitchConfig -> Pitch.Semi
    -> Either BaseTypes.PitchError Pitch.NoteNumber

-- * scale functions

read_note :: DegreeMap -> Pitch.Note -> Either BaseTypes.PitchError Pitch.Pitch
read_note dmap note = maybe (Left BaseTypes.UnparseableNote)
    (Right . semis_to_pitch dmap) $ Map.lookup note (dm_to_semis dmap)

show_pitch :: DegreeMap -> Pitch.Pitch -> Either BaseTypes.PitchError Pitch.Note
show_pitch dmap pitch = maybe (Left BaseTypes.UnparseableNote) Right $
    dm_to_note dmap !? pitch_to_semis dmap pitch

-- ** transpose

transpose :: DegreeMap -> Derive.Transpose
transpose dmap _transposition _environ steps pitch
    | Maybe.isJust $ dm_to_note dmap !? transposed =
        Right $ semis_to_pitch dmap transposed
    | otherwise = Left BaseTypes.out_of_range
    where transposed = pitch_to_semis dmap pitch + steps

-- | Transpose function for a non-transposing scale.
non_transposing :: Derive.Transpose
non_transposing _ _ _ _ = Left BaseTypes.NotImplemented

-- | Indicate that this scale responds to the standard set of transpose
-- signals.  It still has to implement the support in its
-- 'Scale.scale_note_to_call'.
standard_transposers :: Set.Set Score.Control
standard_transposers = Set.fromList
    [ Controls.octave, Controls.chromatic, Controls.diatonic
    , Controls.nn, Controls.hz
    ]

-- ** note_to_call

-- | A specialization of 'note_to_call' that operates on scales with
-- a 'DegreeMap', i.e. a static map from notes to degrees, and from degrees to
-- NNs.
mapped_note_to_call :: DegreeMap -> PSignal.Scale
    -> Pitch.Note -> Maybe Derive.ValCall
mapped_note_to_call dmap scale note = do
    semis <- Map.lookup note (dm_to_semis dmap)
    Just $ note_to_call (dm_per_octave dmap) scale (semis_to_nn semis)
        (semis_to_note semis)
    where
    semis_to_nn semis _config transpose =
        maybe (Left BaseTypes.out_of_range) Right $
            dm_to_nn dmap !? (semis + transpose)
    semis_to_note semis transpose = dm_to_note dmap !? (semis + transpose)

-- | Create a note call that respects chromatic and diatonic transposition.
-- However, diatonic transposition is mapped to chromatic transposition,
-- so this is for scales that don't distinguish.
note_to_call :: Pitch.Semi -> PSignal.Scale -> SemisToNoteNumber
    -> (Pitch.Semi -> Maybe Pitch.Note) -> Derive.ValCall
note_to_call per_octave scale semis_to_nn semis_to_note =
    ScaleDegree.scale_degree scale pitch_nn pitch_note
    where
    pitch_nn :: Scale.PitchNn
    pitch_nn config = to_nn transpose_steps frac config
        where (transpose_steps, frac) = properFraction $ transposition config
    to_nn semis frac config
        | frac == 0 = semis_to_nn config semis
        | otherwise = Num.scale
            <$> semis_to_nn config semis
            <*> semis_to_nn config (semis + 1)
            <*> pure (Pitch.NoteNumber frac)
    pitch_note :: Scale.PitchNote
    pitch_note config =
        maybe (Left BaseTypes.out_of_range) Right $ semis_to_note transpose
        where
        transpose = floor $ transposition config
    transposition config =
        get Controls.octave * fromIntegral per_octave
            + get Controls.chromatic + get Controls.diatonic
        where get c = Map.findWithDefault 0 c (PSignal.pitch_controls config)

add_pc :: DegreeMap -> Pitch.PitchClass -> Pitch.Pitch -> Pitch.Pitch
add_pc dmap = Pitch.add_pc (dm_per_octave dmap)

-- ** input

type InputToNote = Env.Environ -> Pitch.Input
    -> Either BaseTypes.PitchError Pitch.Note

-- | Input to note for simple scales without keys.
input_to_note :: DegreeMap -> InputToNote
input_to_note dmap _environ (Pitch.Input kbd pitch frac) = do
    steps <- simple_kbd_to_scale dmap kbd pitch
    note <- maybe (Left BaseTypes.UnparseableNote) Right $
        dm_to_note dmap !? steps
    return $ ScaleDegree.pitch_expr frac note

type InputToNn = ScoreTime -> Pitch.Input
    -> Derive.Deriver (Either BaseTypes.PitchError Pitch.NoteNumber)

-- | Input to NoteNumber for scales that have a direct relationship between
-- Degree and NoteNumber.
mapped_input_to_nn :: DegreeMap -> InputToNn
mapped_input_to_nn dmap = \_pos (Pitch.Input kbd pitch frac) -> return $ do
    semis <- simple_kbd_to_scale dmap kbd pitch
    maybe (Left BaseTypes.out_of_range) Right $ to_nn semis frac
    where
    to_nn semis frac
        | frac == 0 = lookup semis
        | frac > 0 = do
            nn <- lookup semis
            next <- lookup (semis + 1)
            return $ Num.scale nn next (Pitch.NoteNumber frac)
        | otherwise = do
            nn <- lookup semis
            prev <- lookup (semis - 1)
            return $ Num.scale prev nn (Pitch.NoteNumber (frac + 1))
    lookup semis = dm_to_nn dmap !? semis

set_direct_input_to_nn :: Scale.Scale -> Scale.Scale
set_direct_input_to_nn scale = scale
    { Scale.scale_input_to_nn = direct_input_to_nn }

-- | An Input maps directly to a NoteNumber.  This is an efficient
-- implementation for scales tuned to 12TET.
--
-- This obeys 'Controls.octave' but none of the other transposer controls.
-- This is inconsistent with 'computed_input_to_nn', but trying to implement
-- diatonic transposition would make this not so direct any more.  And in
-- any case, "Cmd.MidiThru" shouldn't let through any transposers other than
-- octave.
direct_input_to_nn :: InputToNn
direct_input_to_nn pos (Pitch.Input _ pitch frac) = do
    controls <- Derive.controls_at =<< Derive.real pos
    let octaves = Map.findWithDefault 0 Controls.octave controls
    return $ Right $ nn + Pitch.nn (frac + octaves * 12)
    where
    nn = fromIntegral $ Theory.semis_to_nn $
        Theory.pitch_to_semis Theory.piano_layout pitch

-- | Convert input to nn by going through note_to_call.  This works for
-- complicated scales that retune based on the environment but is more work.
computed_input_to_nn :: InputToNote -> (Pitch.Note -> Maybe Derive.ValCall)
    -> InputToNn
computed_input_to_nn input_to_note note_to_call pos input = do
    env <- Internal.get_environ
    case get_call env of
        Left err -> return $ Left err
        Right call -> Eval.apply_pitch pos call >>= \val -> case val of
            BaseTypes.VPitch pitch -> do
                controls <- Derive.controls_at =<< Derive.real pos
                nn <- Derive.require_right (("evaluating pich: "<>) . pretty) $
                    PSignal.pitch_nn $ PSignal.coerce $
                    PSignal.config (PSignal.PitchConfig env controls)
                        pitch
                return $ Right nn
            _ -> Derive.throw $ "non-pitch from pitch call: " <> pretty val
    where
    get_call env = do
        note <- input_to_note env input
        maybe (Left BaseTypes.UnparseableNote) Right $ note_to_call note

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Pitch.Frac -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high (Pitch.NoteNumber frac)

-- *** diatonic

simple_kbd_to_scale :: DegreeMap -> Pitch.KbdType -> Pitch.Pitch
    -> Either BaseTypes.PitchError Pitch.Semi
simple_kbd_to_scale dmap kbd pitch =
    pitch_to_semis dmap <$> kbd_to_scale kbd (dm_per_octave dmap) 0 pitch

pitch_to_semis :: DegreeMap -> Pitch.Pitch -> Pitch.Semi
pitch_to_semis dmap (Pitch.Pitch oct (Pitch.Degree pc accs)) =
    (oct - dm_start_octave dmap) * dm_per_octave dmap
        + pc - dm_start_pc dmap + accs

semis_to_pitch :: DegreeMap -> Pitch.Semi -> Pitch.Pitch
semis_to_pitch dmap semis =
    Pitch.add_pc per_oct (dm_start_pc dmap) $
        Pitch.Pitch (oct + dm_start_octave dmap) (Pitch.Degree pc 0)
    where
    (oct, pc) = semis `divMod` per_oct
    per_oct = dm_per_octave dmap

kbd_to_scale :: Pitch.KbdType -> Pitch.PitchClass -> Pitch.PitchClass
    -> Pitch.Pitch -> Either BaseTypes.PitchError Pitch.Pitch
kbd_to_scale kbd pc_per_octave tonic =
    maybe (Left BaseTypes.InvalidInput) Right
    . lookup_kbd_to_scale kbd pc_per_octave tonic

-- | Convert an absolute Pitch in the input keyboard's layout to a relative
-- Pitch within a scale with the given number of diatonic steps per octave, or
-- Nothing if that key should have no pitch.
lookup_kbd_to_scale :: Pitch.KbdType -> Pitch.PitchClass -> Pitch.PitchClass
    -> Pitch.Pitch -> Maybe Pitch.Pitch
lookup_kbd_to_scale kbd pc_per_octave tonic pitch = case kbd of
    Pitch.PianoKbd -> piano_kbd_pitch tonic pc_per_octave pitch
    Pitch.AsciiKbd -> Just $ ascii_kbd_pitch pc_per_octave pitch

-- Scale octave doesn't match the kbd octave, but is absolute:
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
piano_kbd_pitch :: Pitch.PitchClass -> Pitch.PitchClass -> Pitch.Pitch
    -> Maybe Pitch.Pitch
piano_kbd_pitch tonic pc_per_octave (Pitch.Pitch oct (Pitch.Degree pc accs))
    | relative_pc >= pc_per_octave = Nothing
    | otherwise =
        Just $ Pitch.Pitch (oct1 + oct_diff) (Pitch.Degree relative_pc accs)
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
ascii_kbd_pitch :: Pitch.PitchClass -> Pitch.Pitch -> Pitch.Pitch
ascii_kbd_pitch pc_per_octave (Pitch.Pitch oct (Pitch.Degree pc accs)) =
    Pitch.Pitch (add_oct + oct1) (Pitch.Degree pc2 accs)
    where
    (oct1, pc1) = adjust_octave pc_per_octave 10 oct pc
    -- If the scale is shorter than the kbd, go up to the next octave on
    -- the same row.
    (add_oct, pc2) = pc1 `divMod` pc_per_octave

-- | Try to fit a note from a keyboard into a scale.  Round the note up to the
-- nearest multiple of the keyboard octave and adjust the octave accordingly.
adjust_octave :: Pitch.PitchClass -> Pitch.PitchClass -> Pitch.Octave
    -> Pitch.PitchClass -> (Pitch.Octave, Pitch.PitchClass)
adjust_octave pc_per_octave kbd_per_octave oct pc =
    (oct2, offset * kbd_per_octave + pc)
    where
    rows = ceiling $ fromIntegral pc_per_octave / fromIntegral kbd_per_octave
    (oct2, offset) = oct `divMod` rows


-- ** call_doc

call_doc :: Set.Set Score.Control -> DegreeMap -> Doc.Doc
    -> Derive.DocumentedCall
call_doc transposers dmap doc =
    annotate_call_doc transposers doc fields default_scale_degree_doc
    where
    fields
        | Vector.null notes = []
        | otherwise = [("range", Doc.pretty bottom <> " to " <> Doc.pretty top)]
        where
        bottom = notes Vector.! 0
        top = notes Vector.! (Vector.length notes - 1)
        notes = dm_to_note dmap

-- | Documentation of the standard 'Call.Pitch.scale_degree'.
default_scale_degree_doc :: Derive.DocumentedCall
default_scale_degree_doc = scale_degree_doc ScaleDegree.scale_degree

scale_degree_doc ::
    (PSignal.Scale -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall)
    -> Derive.DocumentedCall
scale_degree_doc scale_degree =
    Derive.extract_val_doc $ scale_degree PSignal.no_scale err err
    where err _ = Left $ PSignal.PitchError "it was just an example!"

annotate_call_doc :: Set.Set Score.Control -> Doc.Doc -> [(Doc.Doc, Doc.Doc)]
    -> Derive.DocumentedCall -> Derive.DocumentedCall
annotate_call_doc transposers doc fields = prepend_doc extra_doc
    where
    extra_doc = doc <> "\n\n" <> join (transposers_field ++ fields)
    transposers_field =
        [("transposers", Doc.Doc $ pretty transposers) |
            not (Set.null transposers)]
    join = TextUtil.list
        . map (\(k, v) -> k <> ": " <> v) . filter ((/="") . snd)

add_doc :: Doc.Doc -> Scale.Scale -> Scale.Scale
add_doc doc scale = scale
    { Scale.scale_call_doc = prepend_doc doc (Scale.scale_call_doc scale) }

-- *** DocumentedCall

-- | Prepend a bit of text to the documentation.
prepend_doc :: Doc.Doc -> Derive.DocumentedCall -> Derive.DocumentedCall
prepend_doc text = modify_doc ((text <> "\n") <>)

modify_doc :: (Doc.Doc -> Doc.Doc) -> Derive.DocumentedCall
    -> Derive.DocumentedCall
modify_doc modify (Derive.DocumentedCall name doc) =
    Derive.DocumentedCall name (annotate doc)
    where
    annotate (Derive.CallDoc module_ tags cdoc args) =
        Derive.CallDoc module_ tags (modify cdoc) args

-- * util

no_enharmonics :: Derive.Enharmonics
no_enharmonics _ _ = Right []

-- | Read and parse an environ value, or throw a ScaleError.
read_environ :: (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    (a -> Maybe val) -- ^ parse or Nothing
    -> Maybe val
    -- ^ if Just, a missing value gets this, otherwise it's an error
    -> Env.Key -> Env.Environ -> Either BaseTypes.PitchError val
read_environ read_val maybe_deflt =
    read_environ_ read_val (Right <$> maybe_deflt)

-- | Like 'read_environ', except the default is given to the parse function.
read_environ_default :: (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    (a -> Maybe val) -> Maybe a
    -> Env.Key -> Env.Environ -> Either BaseTypes.PitchError val
read_environ_default read_val maybe_deflt name =
    read_environ_ read_val (parse <$> maybe_deflt) name
    where
    parse val = maybe
        (environ_error ("unexpected default value: " <> ShowVal.show_val val))
        Right (read_val val)
    environ_error = Left . BaseTypes.EnvironError name

read_environ_ :: (Typecheck.Typecheck a, ShowVal.ShowVal a) =>
    (a -> Maybe val) -> Maybe (Either PSignal.PitchError val)
    -> Env.Key -> Env.Environ -> Either BaseTypes.PitchError val
read_environ_ read_val maybe_deflt name env = case Env.get_val name env of
    Left (Env.WrongType expected) ->
        environ_error ("expected type " <> pretty expected)
    Left Env.NotFound -> case maybe_deflt of
        Nothing -> Left $ BaseTypes.EnvironError name "not set"
        Just deflt -> deflt
    Right val -> parse val
    where
    parse val = maybe
        (environ_error ("unexpected value: " <> ShowVal.show_val val))
        Right (read_val val)
    environ_error = Left . BaseTypes.EnvironError name

-- | This is 'read_environ', but for instances of 'Typecheck.TypecheckSymbol'.
parse_environ :: Typecheck.TypecheckSymbol val => Maybe val
    -- ^ if Just, a missing value gets this, otherwise it's an error
    -> Env.Key -> Env.Environ -> Either BaseTypes.PitchError val
parse_environ = read_environ Typecheck.parse_symbol


-- ** keys

environ_key :: Env.Environ -> Maybe Pitch.Key
environ_key = fmap Pitch.Key . Env.maybe_val EnvKey.key

-- | Find a key in a map, or throw a ScaleError.
get_key :: key -> Map.Map Pitch.Key key -> Maybe Pitch.Key
    -> Either BaseTypes.PitchError key
get_key deflt _ Nothing = Right deflt
get_key _ keys (Just key) =
    maybe (Left $ key_error key) Right $ Map.lookup key keys

lookup_key :: key -> Map.Map Pitch.Key key -> Maybe Pitch.Key -> Maybe key
lookup_key deflt _ Nothing = Just deflt
lookup_key _ keys (Just key) = Map.lookup key keys

key_error :: Pitch.Key -> BaseTypes.PitchError
key_error (Pitch.Key key) =
    BaseTypes.EnvironError EnvKey.key ("unknown key: " <> key)
