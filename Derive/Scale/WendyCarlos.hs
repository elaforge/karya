-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Scales invented by Wendy Carlos.  Derived from
-- <http://www.wendycarlos.com/resources/pitch.html>
module Derive.Scale.WendyCarlos (scales) where
import qualified Data.Attoparsec.Text as Attoparsec.Text
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.ParseText as ParseText
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Global


-- TODO letters might be hard to use with so many of them.  Maybe I should use
-- numbers?
scales :: [Scale.Definition]
scales = map Scale.Simple
    [ make "alpha" 19 78
    , make "beta" 19 63.8
    , make "gamma" 35 35.1
    ]
    where
    make scale_id per_octave cents =
        make_scale scale_id $ Vector.fromList $ take per_octave $
            Seq.range_ 0 (cents/100)

type Degrees = Vector.Vector Pitch.NoteNumber

-- | Frequency for a0.
a0_nn :: Score.Control
a0_nn = "a0-nn"

make_scale :: Pitch.ScaleId -> Degrees -> Scale.Scale
make_scale scale_id degrees = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = "[0-9][a-" <> show_degree (per_octave - 1) <> "]"
    , scale_symbols = []
    , scale_transposers = transposers
    , scale_read = const $ read_pitch per_octave
    , scale_show = const (Right . show_pitch)
    , scale_bottom = Pitch.pitch 0 0
    , scale_layout = Scale.diatonic_layout per_octave
    , scale_transpose = transpose per_octave
    , scale_enharmonics = Scales.no_enharmonics
    , scale_note_to_call = note_to_call_
    , scale_input_to_note = input_to_note per_octave
    , scale_input_to_nn =
        Scales.computed_input_to_nn (input_to_note per_octave) note_to_call_
    , scale_call_doc = Scales.annotate_call_doc transposers
        doc [] Scales.default_scale_degree_doc
    }
    where
    transposers = Scales.standard_transposers
    per_octave = Vector.length degrees
    note_to_call_ = note_to_call scale degrees
    scale = PSignal.Scale scale_id transposers
    doc = "This is a family of scales invented by Wendy Carlos. They don't\
        \ repeat at the octave, so they support diatonic transposition only\
        \ within a single octave.\
        \\nSince diatonic transposition won't get you up an octave,\
        \ the " <> ShowVal.doc Controls.octave <> " control will transpose\
        \ by octaves.\
        \\nThe " <> ShowVal.doc a0_nn <> " control sets the NoteNumber of\
        \ `a0`. If it's not set, it defaults to 0, which is `c-1`."

read_pitch :: Pitch.PitchClass -> Pitch.Note
    -> Either BaseTypes.PitchError Pitch.Pitch
read_pitch per_octave note = justErr BaseTypes.UnparseableNote $
    ParseText.maybe_parse (Pitch.pitch <$> ParseText.p_int <*> p_degree)
        (Pitch.note_text note)
    where
    p_degree = do
        c <- Attoparsec.Text.anyChar
        let pc = Char.ord c - Char.ord 'a'
        guard (pc < per_octave)
        return pc

show_pitch :: Pitch.Pitch -> Pitch.Note
show_pitch (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Pitch.Note $ showt oct <> show_degree pc

show_degree :: Pitch.PitchClass -> Text
show_degree pc = Text.singleton $ Char.chr $
    if pc < 26 then Char.ord 'a' + pc else Char.ord 'A' + (pc - 26)

transpose :: Pitch.PitchClass -> Derive.Transpose
transpose per_octave _transposition _key steps pitch
    | Pitch.pitch_pc pitch + steps >= per_octave =
        Left $ BaseTypes.OutOfRangeError BaseTypes.out_of_range
    | otherwise = Right $ Pitch.add_pc per_octave steps pitch

note_to_call :: PSignal.Scale -> Degrees -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call scale degrees note = do
    pitch <- either (const Nothing) Just $ read_pitch per_octave note
    -- Pass 0 for per_octave, since I'll be handling the octave here.
    Just $ Scales.note_to_call 0 (Just max_semi) scale
        (semis_to_nn pitch) (semis_to_note pitch)
    where
    max_semi = Vector.length degrees
    semis_to_nn pitch config semis =
        justErr (BaseTypes.out_of_range_error semis (0, max_semi)) $ do
            let a0 = Pitch.nn $ get a0_nn
            nn <- degrees Vector.!? (Pitch.pitch_pc pitch + semis)
            Just $ a0 + nn
                + Pitch.nn ((Pitch.pitch_octave pitch + octaves) * 12)
        where
        octaves = floor $ get Controls.octave
        get c = Map.findWithDefault 0 c (PSignal.pitch_controls config)
    semis_to_note pitch steps
        | Pitch.pitch_pc pitch + steps >= per_octave = Nothing
        | otherwise = Just $ show_pitch $ Pitch.add_pc per_octave steps pitch
    per_octave = Vector.length degrees

input_to_note :: Pitch.PitchClass -> Scales.InputToNote
input_to_note per_octave _env (Pitch.Input kbd pitch _frac) =
    show_pitch <$> Scales.kbd_to_scale kbd per_octave 0 pitch
