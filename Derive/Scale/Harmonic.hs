-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Harmonic series.
module Derive.Scale.Harmonic where
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch

import           Global

-- TODO There's no easy way to enter negative harmonics.  Maybe I'd need some
-- kind of alternate input mode that could affect input_to_note.

scales :: [Scale.Definition]
scales = (:[]) $ Scale.Simple $ Scale.Scale
    { scale_id = scale_id
    , scale_pattern = "[" <> txt octaves <> "]-?[0-9]+"
    , scale_symbols = []
    , scale_transposers = transposers
    , scale_read = const $ read_pitch
    , scale_show = const (Right . show_pitch)
    , scale_bottom = Pitch.pitch 0 0
    -- This scale doesn't actually have a layout, but for the purposes of
    -- Pitch.Pitch math, it acts like alternating white and black keys.
    , scale_layout = Scale.layout (replicate 20 2)
    , scale_transpose = transpose
    , scale_enharmonics = Scales.no_enharmonics
    , scale_note_to_call = note_to_call scale
    , scale_input_to_note = input_to_note
    , scale_input_to_nn =
        Scales.computed_input_to_nn input_to_note (note_to_call scale)
    , scale_call_doc = Scales.annotate_call_doc transposers
        doc [] Scales.default_scale_degree_doc
    }
    where
    transposers = Scales.standard_transposers
    doc = "The harmonic series. The NN frequency of 一1 is set by "
        <> ShowVal.doc unity_control <> ", and defaults to 0nn. This puts\
        \ 五1 at middle C, which is where 四1 would normally be, but since the\
        \ first degree is the octave, putting 四1 at middle C would get too\
        \ high too quickly.\n\
        \ Negative numbers are the nth undertone, i.e. `1 / abs n`."
    scale = PSignal.Scale scale_id transposers
    scale_id = "harmonic"

-- | Frequency for 一1.
unity_control :: ScoreT.Control
unity_control = "unity"

read_pitch :: Pitch.Note -> Either BaseTypes.PitchError Pitch.Pitch
read_pitch note = justErr BaseTypes.UnparseableNote $
    ParseText.maybe_parse (Pitch.pitch <$> p_octave <*> ParseText.p_int)
        (Pitch.note_text note)
    where
    p_octave = do
        c <- A.anyChar
        maybe mzero (return . (+1)) $ List.elemIndex c octaves

octaves :: [Char]
octaves = "一二三四五六七八久十"

show_pitch :: Pitch.Pitch -> Pitch.Note
show_pitch (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Pitch.Note $ oct_s <> showt pc
    where
    oct_s = maybe (showt oct <> "-") Text.singleton (Seq.at octaves (oct-1))

transpose :: Derive.Transpose
transpose _transposition _env steps (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Right $ Pitch.Pitch oct (Pitch.Degree (pc + steps) 0)

note_to_call :: PSignal.Scale -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale note = do
    pitch <- either (const Nothing) Just $ read_pitch note
    -- Pass 0 for per_octave, since I'll be handling the octave here.
    Just $ Scales.note_to_call 0 Nothing scale
        (semis_to_nn pitch) (semis_to_note pitch)
    where
    semis_to_nn (Pitch.Pitch oct (Pitch.Degree pc _)) config semis =
        Right $ Pitch.hz_to_nn hz
        where
        hz = Pitch.nn_to_hz (Pitch.nn (get unity_control))
            * 2^(oct + floor (get Controls.octave))
            * harmonic (pc + semis)
        harmonic n
            | n == 0 = 1
            | n > 0 = fromIntegral n
            | otherwise = 1 / fromIntegral (abs n)
        get c = Map.findWithDefault 0 c (PSignal.pitch_controls config)
    semis_to_note pitch steps = Just $ show_pitch $ add_pc steps pitch
    add_pc steps (Pitch.Pitch oct (Pitch.Degree pc accs)) =
        Pitch.Pitch oct (Pitch.Degree (pc + steps) accs)

input_to_note :: Scales.InputToNote
input_to_note _ input =
    show_pitch <$> justErr BaseTypes.InvalidInput (input_to_pitch input)

input_to_pitch :: Pitch.Input -> Maybe Pitch.Pitch
input_to_pitch (Pitch.Input kbd p@(Pitch.Pitch oct (Pitch.Degree pc acc)) _) =
    case kbd of
        Pitch.PianoKbd -> expand <$> Scales.piano_kbd_pitch 0 14 p
        -- This gives 19 harmonics on the bottom row, and 18 on the top row.
        Pitch.AsciiKbd -> Just $ Pitch.pitch oct (pc * 2 + acc + 1)
    where
    -- Make black keys into diatonic steps, so I can get 24 harmonics in a two
    -- octave range.
    expand (Pitch.Pitch oct degree) =
        Pitch.pitch oct (Theory.degree_to_semis Theory.piano_layout degree)
