-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Harmonic series.
module Derive.Scale.Harmonic where
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.ParseText as ParseText
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
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
    { scaleId = scaleId
    , scalePattern = "[" <> txt octaves <> "]-?[0-9]+"
    , scaleSymbols = []
    , scaleTransposers = transposers
    , scaleRead = const $ readPitch
    , scaleShow = const (Right . showPitch)
    , scaleBottom = Pitch.pitch 0 0
    -- This scale doesn't actually have a layout, but for the purposes of
    -- Pitch.Pitch math, it acts like alternating white and black keys.
    , scaleLayout = Scale.layout (replicate 20 2)
    , scaleTranspose = transpose
    , scaleEnharmonics = Scales.noEnharmonics
    , scaleNoteToCall = noteToCall scale
    , scaleInputToNote = inputToNote
    , scaleInputToNn =
        Scales.computedInputToNn inputToNote (noteToCall scale)
    , scaleCallDoc = Scales.annotateCallDoc transposers
        doc [] Scales.defaultScaleDegreeDoc
    }
    where
    transposers = Scales.standardTransposers
    doc = "The harmonic series. The NN frequency of 一1 is set by "
        <> ShowVal.doc unityControl <> ", and defaults to 0nn. This puts\
        \ 五1 at middle C, which is where 四1 would normally be, but since the\
        \ first degree is the octave, putting 四1 at middle C would get too\
        \ high too quickly.\n\
        \ Negative numbers are the nth undertone, i.e. `1 / abs n`."
    scale = PSignal.Scale scaleId transposers
    scaleId = "harmonic"

-- | Frequency for 一1.
unityControl :: ScoreT.Control
unityControl = "unity"

readPitch :: Pitch.Note -> Either DeriveT.PitchError Pitch.Pitch
readPitch note = justErr (DeriveT.UnparseableNote note) $
    ParseText.maybeParse (Pitch.pitch <$> pOctave <*> ParseText.pInt)
        (Pitch.noteText note)
    where
    pOctave = do
        c <- A.anyChar
        maybe mzero (pure . (+1)) $ List.elemIndex c octaves

octaves :: [Char]
octaves = "一二三四五六七八久十"

showPitch :: Pitch.Pitch -> Pitch.Note
showPitch (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Pitch.Note $ octS <> showt pc
    where
    octS = maybe (showt oct <> "-") Text.singleton (Lists.at octaves (oct-1))

transpose :: Derive.Transpose
transpose _transposition _env steps (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Right $ Pitch.Pitch oct (Pitch.Degree (pc + steps) 0)

noteToCall :: PSignal.Scale -> Pitch.Note -> Maybe Derive.ValCall
noteToCall scale note = do
    pitch <- either (const Nothing) Just $ readPitch note
    -- Pass 0 for per_octave, since I'll be handling the octave here.
    Just $ Scales.noteToCall 0 Nothing scale
        (semisToNn pitch) (semisToNote pitch)
    where
    semisToNn (Pitch.Pitch oct (Pitch.Degree pc _)) config semis =
        Right $ Pitch.hzToNn hz
        where
        hz = Pitch.nnToHz (Pitch.nn (get unityControl))
            * 2^(oct + floor (get Controls.octave))
            * harmonic (pc + semis)
        harmonic n
            | n == 0 = 1
            | n > 0 = fromIntegral n
            | otherwise = 1 / fromIntegral (abs n)
        get c = Map.findWithDefault 0 c (PSignal.pitchControls config)
    semisToNote pitch steps = Just $ showPitch $ addPc steps pitch
    addPc steps (Pitch.Pitch oct (Pitch.Degree pc accs)) =
        Pitch.Pitch oct (Pitch.Degree (pc + steps) accs)

inputToNote :: Scales.InputToNote
inputToNote _ input =
    showPitch <$> justErr DeriveT.InvalidInput (inputToPitch input)

inputToPitch :: Pitch.Input -> Maybe Pitch.Pitch
inputToPitch (Pitch.Input kbd p@(Pitch.Pitch oct (Pitch.Degree pc acc)) _) =
    case kbd of
        Pitch.PianoKbd -> expand <$> Scales.pianoKbdPitch 0 14 p
        -- This gives 19 harmonics on the bottom row, and 18 on the top row.
        Pitch.AsciiKbd -> Just $ Pitch.pitch oct (pc * 2 + acc + 1)
    where
    -- Make black keys into diatonic steps, so I can get 24 harmonics in a two
    -- octave range.
    expand (Pitch.Pitch oct degree) =
        Pitch.pitch oct (Theory.degreeToSemis Theory.pianoLayout degree)
