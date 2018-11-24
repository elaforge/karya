-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Util where
import qualified Control.Monad.Except as Except
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Osc as Osc
import qualified Synth.Shared.Signal as Signal

import Global


-- * preprocess

nextsBy :: Eq key => (a -> key) -> [a] -> [(a, [a])]
nextsBy key xs = zipWith extract xs (drop 1 (List.tails xs))
    where extract x xs = (x, filter ((== key x) . key) xs)

nexts :: [a] -> [(a, [a])]
nexts xs = zip xs (drop 1 (List.tails xs))


-- * convert

symbolicPitch :: (Except.MonadError Text m) => Note.Note
    -> m (Either Pitch.Note Pitch.NoteNumber)
symbolicPitch note
    | Text.null (Note.element note) =
        Right <$> tryJust "no pitch" (Note.initialPitch note)
    | otherwise = return $ Left $ Pitch.Note $ Note.element note

articulation :: a -> Common.AttributeMap a -> Attrs.Attributes -> a
articulation deflt attributeMap  =
    maybe deflt snd . (`Common.lookup_attributes` attributeMap)

-- | Get patch-specific dyn category, and note dynamic.
--
dynamic :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Signal.Y
    -- ^ Min dyn.  This is for normalized samples, where 0 gets this dyn.
    -> Note.Note -> (dyn, Signal.Y)
dynamic dynToRange minDyn note =
    (fst $ findDynamic dynToRange dyn, Num.scale minDyn 1 dyn)
    where dyn = fromMaybe 0 $ Note.initial Control.dynamic note

dynamicScale :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Note.Note
    -> (dyn, Signal.Y)
dynamicScale dynToRange =
    findDynamic dynToRange . fromMaybe 0 . Note.initial Control.dynamic

-- | Convert to (Dynamic, DistanceFromPrevDynamic)
findDynamic :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Signal.Y
    -> (dyn, Signal.Y)
findDynamic dynToRange y =
    find 0 (Num.clamp 0 127 (round (y * 127))) rangeDynamics
    where
    find low val ((high, dyn) : rest)
        | null rest || val < high =
            (dyn, Num.normalize (int low) (int high) (int val))
        | otherwise = find high val rest
    find _ _ [] = error "empty rangeDynamics"
    int = fromIntegral
    rangeDynamics = Seq.key_on (snd . dynToRange) enumAll

type Variation = Int

variation :: Variation -> Note.Note -> Variation
variation variations = pick . fromMaybe 0 . Note.initial Control.variation
    where pick var = round (var * fromIntegral (variations - 1))

-- * thru

thru :: FilePath -> (Note.Note -> Patch.ConvertM (a, Sample.Sample))
    -> ImInst.Code
thru dir convert = ImInst.thru $ thruFunction dir convert

thruFunction :: FilePath -> (Note.Note -> Patch.ConvertM (a, Sample.Sample))
    -> Osc.ThruFunction
thruFunction dir convert attrs pitch velocity = do
    ((_, sample), _logs) <- Patch.runConvert $ convert $ (Note.note "" "" 0 1)
        { Note.attributes = attrs
        , Note.controls = Map.fromList
            [ (Control.pitch, Signal.constant (Pitch.nn_to_double pitch))
            , (Control.dynamic, Signal.constant velocity)
            ]
        }
    return [Sample.toOsc dir sample]

-- * util

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

findBelow :: Ord k => (a -> k) -> k -> [a] -> a
findBelow _ _ [] = error "empty list"
findBelow key val (x:xs) = go x xs
    where
    go x0 (x:xs)
        | val < key x = x0
        | otherwise = go x xs
    go x0 [] = x0
