-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Util where
import qualified Control.Monad.Except as Except
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.CUtil as CUtil
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

symbolicPitch :: Except.MonadError Text m => Note.Note
    -> m (Either Pitch.Note Pitch.NoteNumber)
symbolicPitch note
    | Text.null (Note.element note) = Right <$> initialPitch note
    | otherwise = return $ Left $ Pitch.Note $ Note.element note

initialPitch :: Except.MonadError Text m => Note.Note -> m Pitch.NoteNumber
initialPitch = tryJust "no pitch" . Note.initialPitch

articulation :: Except.MonadError Text m => Common.AttributeMap a
    -> Attrs.Attributes -> m a
articulation attributeMap  =
    maybe (Except.throwError "no attribute") (return . snd)
    . (`Common.lookup_attributes` attributeMap)

articulationDefault :: a -> Common.AttributeMap a -> Attrs.Attributes -> a
articulationDefault deflt attributeMap  =
    maybe deflt snd . (`Common.lookup_attributes` attributeMap)

-- | Get patch-specific dyn category, and note dynamic.
dynamic :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Signal.Y
    -- ^ Min dyn.  This is for normalized samples, where 0 gets this dyn.
    -> Note.Note -> (dyn, Signal.Y)
dynamic dynToRange minDyn note =
    (fst $ findDynamic dynToRange dyn, Num.scale minDyn 1 dyn)
    where dyn = Note.initial0 Control.dynamic note

dynamicScale :: (Bounded dyn, Enum dyn) => (dyn -> (Int, Int)) -> Note.Note
    -> (dyn, Signal.Y)
dynamicScale dynToRange = findDynamic dynToRange . Note.initial0 Control.dynamic

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
variation variations = pick . Note.initial0 Control.variation
    where pick var = round (var * fromIntegral (variations - 1))

chooseVariation :: [a] -> Note.Note -> a
chooseVariation xs = pick . Note.initial0 Control.variation
    where pick var = xs !! round (var * fromIntegral (length xs - 1))

-- * thru

thru :: FilePath -> (Note.Note -> Patch.ConvertM Sample.Sample)
    -> ImInst.Code
thru sampleDir convert = ImInst.thru $ thruFunction sampleDir convert

imThruFunction :: FilePath -> (Note.Note -> Patch.ConvertM Sample.Sample)
    -> CUtil.Thru
imThruFunction dir = CUtil.ImThru . thruFunction dir

thruFunction :: FilePath -> (Note.Note -> Patch.ConvertM Sample.Sample)
    -> Osc.ThruFunction
thruFunction sampleDir convert attrs pitch velocity = do
    (sample, _logs) <- Patch.runConvert $ convert $ (Note.note "" "" 0 1)
        { Note.attributes = attrs
        , Note.controls = Map.fromList
            [ (Control.pitch, Signal.constant (Pitch.nn_to_double pitch))
            , (Control.dynamic, Signal.constant velocity)
            ]
        }
    return [Sample.toOsc sampleDir sample]

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

showLower :: Show a => a -> String
showLower = map Char.toLower . show

showtLower :: Show a => a -> Text
showtLower = txt . showLower
