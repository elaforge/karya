-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Utilities for Balinese instruments.
module Synth.Sampler.Patch.Lib.Bali where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Text.Read as Read

import qualified Util.Num as Num
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.C.Bali.Gender as Gender
import qualified Derive.C.Prelude.Note as Prelude.Note
import qualified Derive.Call as Call
import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.DUtil as DUtil

import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


data Pitch = Pitch Pitch.Octave PitchClass
    deriving (Eq, Ord, Show)

instance Pretty Pitch where
    pretty (Pitch oct pc) = showt oct <> Util.showtLower pc

instance Enum Pitch where
    toEnum n = let (oct, pc) = n `divMod` 5 in Pitch oct (toEnum pc)
    fromEnum (Pitch oct pc) = oct * 5 + fromEnum pc

data PitchClass = I | O | E | U | A
    deriving (Eq, Ord, Show, Enum, Bounded, Read)

parsePitch :: String -> Maybe Pitch
parsePitch [oct, pc] = Pitch <$> Num.readDigit oct
    <*> Read.readMaybe (Char.toUpper pc : "")
parsePitch _ = Nothing

-- * code

zeroDurMute :: Util.Dyn -> ImInst.Code
zeroDurMute dyn = Bali.zeroDurMuteWith ""
    (\_args -> transform . Call.multiplyDynamic dyn)
    (\args -> transform $
        Prelude.Note.defaultNote Prelude.Note.useAttributes args)
    where transform = Code.withVariation

wayangCode :: ImInst.Code
wayangCode = ImInst.nullCall weakNote

-- TODO what's the difference with zeroDurMute?
weakNote :: Derive.Generator Derive.Note
weakNote = DUtil.zeroDuration "note"
    "When zero duration, and use-weak=t, use the `weak` call.\
    \ When `symbolic-pitch=t`, tell the samples to use pitch by name rather\
    \ than nn."
    (Sub.inverting $ \args -> transform $ Call.ifEnv "use-weak" (Just True)
        (Gender.weakCall args)
        (Call.multiplyDynamic 0.65 (Bali.reapplyMute args)))
    (Sub.inverting $ \args -> transform (note args))
    where
    note = Prelude.Note.defaultNote Prelude.Note.useAttributes
    transform = Code.withVariation

-- * convert

supportVariableMute :: Im.Patch.Patch -> Im.Patch.Patch
supportVariableMute patch = patch
    { Im.Patch.patchControls = Im.Patch.patchControls patch <>
        Map.singleton Control.mute
            "Variable amount of mute. Simulated with a shortened envelope."
    }

variableMuteEnv :: Util.Dyn -> Note.Note -> Signal.Signal
variableMuteEnv dynVal note
    | mute > 0 = Util.triggerRelease dynVal release note
    | otherwise = Util.sustainRelease dynVal dampTime note
    where
    mute = RealTime.seconds $ Note.initial0 Control.mute note
    release = uncurry Num.scale variableMuteRange (1-mute)
    -- Duration of shortest to longest mute.
    variableMuteRange = (0.25, 1)
    -- Time to damp a note to 0.
    dampTime = 0.35
