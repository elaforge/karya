-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- enumFunction
{- | Utilities shared between drum patches.

    The base structure is that a drum has an enumeration of articulations,
    where each one is a directory of samples of increasing dynamics.  There
    are an arbitrary number of samples, which may or may not be normalized,
    which form a continuum, rather than having explicit dynamic groups.  Since
    there are no explicit variation samples, variation takes neighbor dynamics,
    where the variation range is defined per-patch.

    There is a 'Common.AttributeMap' mapping attrs to each articulation.
-}
module Synth.Sampler.Patch.Lib.Drum where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Data.Typeable as Typeable

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Text.Read as Read

import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Expr as Expr
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


data ConvertMap art = ConvertMap {
    -- | Dyn from 0 to 1 will be scaled to this range.  If the samples are not
    -- normalized, and there are enough for a smooth curve, then (1, 1) should
    -- do.
    --
    -- TODO: if the samples are not normalized, I need a separate range for
    -- each sample, to smooth out the differences.
    _dynRange :: (Signal.Y, Signal.Y)
    -- | If Just, use the note's pitch, assuming ratio=1 will be this pitch.
    ,  _naturalNn :: Maybe (art -> Pitch.NoteNumber)
    -- | Time to mute at the end of a note.
    , _muteTime :: Maybe RealTime.RealTime
    , _convertAttributeMap :: Common.AttributeMap art
    -- | articulation -> dynamic -> variation -> (FilePath, (lowDyn, highDyn)).
    -- Returning the sample's dyn range was an attempt to tune dyn globally,
    -- but I think it doesn't work, see TODO above.
    , _getFilename :: art -> Signal.Y -> Signal.Y
        -> (FilePath, Maybe (Signal.Y, Signal.Y))
    }

-- | Create a '_getFilename' with the strategy where each articulation has
-- a @[FilePath]@, sorted evenly over the dynamic range.
variableDynamic :: Show art
    -- | A note may pick a sample of this much dyn difference on either side.
    => Signal.Y -> (art -> [FilePath])
    -> (art -> Signal.Y -> Signal.Y -> (FilePath, Maybe a))
variableDynamic variationRange articulationSamples = \art dyn var ->
    (, Nothing) $
    show art </> Util.pickDynamicVariation variationRange
        (articulationSamples art) dyn var

-- | Make a generic convert, suitable for drum type patches.
convert :: ConvertMap art -> Note.Note -> Patch.ConvertM Sample.Sample
convert (ConvertMap (minDyn, maxDyn) naturalNn muteTime attributeMap
        getFilename) =
    \note -> do
        articulation <- Util.articulation attributeMap (Note.attributes note)
        let dyn = Note.initial0 Control.dynamic note
        let var = maybe 0 (subtract 1 . (*2)) $
                Note.initial Control.variation note
        let (filename, mbDynRange) = getFilename articulation dyn var
        let noteDyn = case mbDynRange of
                Nothing -> Num.scale minDyn maxDyn dyn
                Just dynRange ->
                    Util.dynamicAutoScale (minDyn, maxDyn) dynRange dyn
        ratio <- case naturalNn of
            Nothing -> return 1
            Just artNn -> Sample.pitchToRatio (artNn articulation) <$>
                Util.initialPitch note
        return $ (Sample.make filename)
            { Sample.envelope = case muteTime of
                Nothing -> Signal.constant noteDyn
                Just time -> Util.sustainRelease noteDyn time note
            , Sample.ratios = Signal.constant ratio
            }

patch :: ConvertMap art -> ImInst.Patch
patch convertMap = patch_
    (const () <$> _convertAttributeMap convertMap)
    (Maybe.isJust (_naturalNn convertMap))

patch_ :: Common.AttributeMap () -> Bool -> ImInst.Patch
patch_ attributeMap hasNaturalNn = ImInst.make_patch $ Im.Patch.patch
    { Im.Patch.patch_controls = mconcat
        [ Control.supportDyn
        , Control.supportVariation
        , if hasNaturalNn then Control.supportPitch else mempty
        ]
    , Im.Patch.patch_attribute_map = attributeMap
    }

-- * StrokeMap

-- | Describe a drum-like instrument.  This is just the data for the various
-- functions to construct the patch.
data StrokeMap art = StrokeMap {
    -- | Map each articulation to the articulations that stop it.
    _stops :: Map art (Set art)
    , _strokes :: [Drums.Stroke]
    , _articulations :: [Maybe art]
    , _attributeMap :: Common.AttributeMap art
    } deriving (Show)

-- | Make a StrokeMap from a table with all the relevant info.
strokeMapTable :: Ord art => Drums.Stops
    -> [(Char, Expr.Symbol, Attrs.Attributes, art, Drums.Group)]
    -> StrokeMap art
strokeMapTable stops table = StrokeMap
    { _stops = stopMap [(art, group) | (_, _, _, art, group) <- table] stops
    , _strokes = map makeStroke table
    , _articulations = [Just art | (_, _, _, art, _) <- table]
    , _attributeMap = Common.attribute_map
        [(attrs, art) | (_, _, attrs, art, _) <- table]
    }
    where
    makeStroke (key, call, attrs, _, group) = Drums.Stroke
        { _name = call
        , _attributes = attrs
        , _char = key
        , _dynamic = 1
        , _group = group
        }

-- | Make a StrokeMap from separate strokes and AttributeMap.  This happens
-- when instruments parts are factored apart, due to having both MIDI and
-- im versions.
strokeMap :: Ord art => Drums.Stops -> [Drums.Stroke]
    -> Common.AttributeMap art -> StrokeMap art
strokeMap stops strokes attributeMap = StrokeMap
    { _stops = stopMap artToGroup stops
    , _strokes = strokes
    , _articulations = map strokeToArt strokes
    , _attributeMap = attributeMap
    }
    where
    -- This is awkward because I want to preserve the art, but unlike
    -- 'strokeMapTable', I can't guarantee a 1:1 attr:art mapping.
    strokeToArt attr = fmap snd
        . (`Common.lookup_attributes` attributeMap)
        . Drums._attributes $ attr
    artToGroup = do
        stroke <- strokes
        art <- maybe [] ((:[]) . snd) $
            Common.lookup_attributes (Drums._attributes stroke) attributeMap
        return (art, Drums._group stroke)

stopMap :: Ord art => [(art, Drums.Group)]
    -> [(Drums.Group, [Drums.Group])] -> Map art (Set art)
stopMap artToGroup closedOpens =
    Map.fromList $ filter (not . Set.null . snd) $
        map (fmap groupToStops) artToGroup
    where
    groupToStops =
        Set.fromList . concatMap (Maps.getM toArts) . Maps.getM toStops
    toArts = Maps.multimap (map Tuple.swap artToGroup)
    toStops = Maps.multimap
        [ (open, closed)
        | (closed, opens) <- closedOpens, open <- opens
        ]

-- * inferDuration

-- | Notes ring until stopped by their stop note.
inferDuration :: Ord art => StrokeMap art -> [Note.Note] -> [Note.Note]
inferDuration strokeMap = map infer . Util.nexts
    where
    infer (note, nexts) = note
        { Note.duration = inferEnd strokeMap note nexts - Note.start note }

inferEnd :: Ord art => StrokeMap art -> Note.Note -> [Note.Note]
    -> RealTime.RealTime
inferEnd strokeMap note nexts =
    case List.find (maybe False (`Set.member` stops) . noteArt) nexts of
        Nothing -> Sample.forever
        Just stop -> Note.start stop
    where
    stops = maybe mempty (Maps.getM (_stops strokeMap)) (noteArt note)
    noteArt = fmap snd . (`Common.lookup_attributes` attributeMap)
        . Note.attributes
    attributeMap = _attributeMap strokeMap

-- * file list

-- | Generate haskell code for an Articulation -> [FilePath] function.
--
-- This expects a subdirectory for each articulation, whose name is the same
-- as the Articulation constructor, and sorts it on the last numeric
-- dash-separated field.
--
-- This could be done with TH but it's constant so it's simpler to copy paste
-- into the source.
makeFileList :: FilePath -> [FilePath] -> String -> IO ()
makeFileList dir articulations variableName = do
    putStrLn $ variableName <> " :: Articulation -> [FilePath]"
    putStrLn $ variableName <> " = \\case"
    forM_ articulations $ \art -> do
        fns <- Seq.sort_on filenameSortKey <$>
            Directory.listDirectory (Config.unsafeSamplerRoot </> dir </> art)
        putStrLn $ indent <> art <> " ->"
        putStrLn $ indent2 <> "[ " <> show (head fns)
        mapM_ (\fn -> putStrLn $ indent2 <> ", " <> show fn) (tail fns)
        putStrLn $ indent2 <> "]"
    where
    indent = replicate 4 ' '
    indent2 = indent <> indent

filenameSortKey :: FilePath -> Int
filenameSortKey fname =
    fromMaybe (error $ "can't parse " <> fname) (parse fname)
    where
    parse = Seq.head . mapMaybe Read.readMaybe . reverse . Seq.split "-"
        . FilePath.dropExtension

-- | Emit haskell code for a function from an Enum to lists.  @Enum a@ is
-- redundant according to ghc, but I want it anyway.
enumFunction
    :: (Enum a, Typeable.Typeable a, Show a, Typeable.Typeable b, Pretty b)
    => String -> [(a, [b])] -> [String]
enumFunction name abs@((a0, b0) : _) =
    [ name <> " :: " <> show (Typeable.typeOf a0)
        <> " -> " <> show (Typeable.typeOf b0)
    , name <> " = \\case"
    ] ++ concatMap (indent . makeCase) abs
    where
    makeCase (a, bs) = show a <> " ->"
        : indent (Seq.map_head_tail ("[ "<>) (", "<>) (map prettys bs) ++ ["]"])
    indent = map (replicate 4 ' ' <>)
enumFunction name _ = error $ "function has no values: " <> name
