-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Reyong (patches) where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Sampler.Patch2 as Patch2
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


patches :: [Patch2.Patch]
patches = [patch]

patch :: Patch2.Patch
patch = Patch2.Patch
    { _name = "reyong"
    , _convert = convert
    , _karyaPatch = ImInst.make_patch $ Im.Patch.Patch
        { patch_controls = mempty
        , patch_attribute_map = const () <$> attributeMap
        , patch_flags = mempty
        }
    }

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (cek <> open, CekOpen)
    , (cek, CekClosed)
    , (mute <> open, MuteOpen)
    , (mute, MuteClosed)
    , (mempty, Open)
    ]
    where
    mute = Attrs.mute
    open = Attrs.open
    -- TODO from Derive.C.Bali.Reyong, or from a common attrs module
    cek = Attrs.attr "cek"

-- * convert

convert :: Note.Note -> Either Text Sample.Sample
convert note = do
    let articulation = convertArticulation $ Note.attributes note
    let (dyn, scale) = convertDynamic $ fromMaybe 0 $
            Note.initial Control.dynamic note
    return $ Left "reyong incomplete"

{-
    45-1-31-cek+{closed,open}+v{1..6}.wav
    45-1-31-mute+{closed,open}+v{1..4}.wav
    45-109-127-open+v{1..4}.wav

    keys: 45 48 50 52 55 57 60 62 64 67 69 72 74 76 79
-}


data Articulation = CekClosed | CekOpen | MuteClosed | MuteOpen | Open
    deriving (Eq, Ord, Show)

-- * TODO copy paste from Wayang

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Enum, Bounded, Show)
type Variation = Int

convertArticulation :: Attrs.Attributes -> Articulation
convertArticulation = maybe Open snd . (`Common.lookup_attributes` attributeMap)

-- | Convert to (Dynamic, DistanceFromPrevDynamic)
convertDynamic :: Signal.Y -> (Dynamic, Signal.Y)
convertDynamic y = find 0 (Num.clamp 0 127 (round (y * 127))) rangeDynamics
    where
    find low val ((high, dyn) : rest)
        | null rest || val < high =
            (dyn, Num.normalize (int low) (int high) (int val))
        | otherwise = find high val rest
    find _ _ [] = error "empty rangeDynamics"
    int = fromIntegral
    rangeDynamics = Seq.key_on (snd . dynamicRange) enumAll

convertVariation :: Note.Note -> Variation
convertVariation = maybe 0 floor . Note.initial Control.variation

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

dynamicRange :: Dynamic -> (Int, Int)
dynamicRange = \case
    PP -> (1, 31)
    MP -> (32, 64)
    MF -> (65, 108)
    FF -> (109, 127)
