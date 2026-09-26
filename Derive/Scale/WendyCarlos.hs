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
import qualified Util.Lists as Lists
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch

import           Global


-- TODO letters might be hard to use with so many of them.  Maybe I should use
-- numbers?
scales :: [Scale.Definition]
scales = map Scale.Simple
    [ make "alpha" 19 78
    , make "beta" 19 63.8
    , make "gamma" 35 35.1
    ]
    where
    make scaleId perOctave cents =
        makeScale scaleId $ Vector.fromList $ take perOctave $
            Lists.range_ 0 (cents/100)

type Degrees = Vector.Vector Pitch.NoteNumber

-- | Frequency for a0.
a0Nn :: ScoreT.Control
a0Nn = "a0-nn"

makeScale :: Pitch.ScaleId -> Degrees -> Scale.Scale
makeScale scaleId degrees = Scale.Scale
    { scaleId = scaleId
    , scalePattern = "[0-9][a-" <> showDegree (perOctave - 1) <> "]"
    , scaleSymbols = []
    , scaleTransposers = transposers
    , scaleRead = const $ readPitch perOctave
    , scaleShow = const (Right . showPitch)
    , scaleBottom = Pitch.pitch 0 0
    , scaleLayout = Scale.diatonicLayout perOctave
    , scaleTranspose = transpose perOctave
    , scaleEnharmonics = Scales.noEnharmonics
    , scaleNoteToCall = noteToCall_
    , scaleInputToNote = inputToNote perOctave
    , scaleInputToNn =
        Scales.computedInputToNn (inputToNote perOctave) noteToCall_
    , scaleCallDoc = Scales.annotateCallDoc transposers
        doc [] Scales.defaultScaleDegreeDoc
    }
    where
    transposers = Scales.standardTransposers
    perOctave = Vector.length degrees
    noteToCall_ = noteToCall scale degrees
    scale = PSignal.Scale scaleId transposers
    doc = "This is a family of scales invented by Wendy Carlos. They don't\
        \ repeat at the octave, so they support diatonic transposition only\
        \ within a single octave.\
        \\nSince diatonic transposition won't get you up an octave,\
        \ the " <> ShowVal.doc Controls.octave <> " control will transpose\
        \ by octaves.\
        \\nThe " <> ShowVal.doc a0Nn <> " control sets the NoteNumber of\
        \ `a0`. If it's not set, it defaults to 0, which is `c-1`."

readPitch :: Pitch.PitchClass -> Pitch.Note
    -> Either DeriveT.PitchError Pitch.Pitch
readPitch perOctave note = justErr (DeriveT.UnparseableNote note) $
    ParseText.maybeParse (Pitch.pitch <$> ParseText.pInt <*> pDegree)
        (Pitch.noteText note)
    where
    pDegree = do
        c <- Attoparsec.Text.anyChar
        let pc = Char.ord c - Char.ord 'a'
        guard (pc < perOctave)
        pure pc

showPitch :: Pitch.Pitch -> Pitch.Note
showPitch (Pitch.Pitch oct (Pitch.Degree pc _)) =
    Pitch.Note $ showt oct <> showDegree pc

showDegree :: Pitch.PitchClass -> Text
showDegree pc = Text.singleton $ Char.chr $
    if pc < 26 then Char.ord 'a' + pc else Char.ord 'A' + (pc - 26)

transpose :: Pitch.PitchClass -> Derive.Transpose
transpose perOctave _transposition _key steps pitch
    | Pitch.pitchPc pitch + steps >= perOctave =
        Left $ DeriveT.OutOfRangeError DeriveT.outOfRange
    | otherwise = Right $ Pitch.addPc perOctave steps pitch

noteToCall :: PSignal.Scale -> Degrees -> Pitch.Note
    -> Maybe Derive.ValCall
noteToCall scale degrees note = do
    pitch <- either (const Nothing) Just $ readPitch perOctave note
    -- Pass 0 for per_octave, since I'll be handling the octave here.
    Just $ Scales.noteToCall 0 (Just maxSemi) scale
        (semisToNn pitch) (semisToNote pitch)
    where
    maxSemi = Vector.length degrees
    semisToNn pitch config semis =
        justErr (DeriveT.outOfRangeError semis (0, maxSemi)) $ do
            let a0 = Pitch.nn $ get a0Nn
            nn <- degrees Vector.!? (Pitch.pitchPc pitch + semis)
            Just $ a0 + nn
                + Pitch.nn ((Pitch.pitchOctave pitch + octaves) * 12)
        where
        octaves = floor $ get Controls.octave
        get c = Map.findWithDefault 0 c (PSignal.pitchControls config)
    semisToNote pitch steps
        | Pitch.pitchPc pitch + steps >= perOctave = Nothing
        | otherwise = Just $ showPitch $ Pitch.addPc perOctave steps pitch
    perOctave = Vector.length degrees

inputToNote :: Pitch.PitchClass -> Scales.InputToNote
inputToNote perOctave _env (Pitch.Input kbd pitch _frac) =
    showPitch <$> Scales.kbdToScale kbd perOctave 0 pitch
