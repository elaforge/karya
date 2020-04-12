-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Text.Read as Read

import qualified Util.Maps as Maps
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.Drums as Drums
import qualified Derive.Attrs as Attrs
import qualified Derive.Expr as Expr
import qualified Instrument.Common as Common
import qualified Perform.RealTime as RealTime
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import           Global


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
        putStrLn $ "    " <> art <> " ->"
        let indent = replicate 8 ' '
        putStrLn $ indent <> "[ " <> show (head fns)
        mapM_ (\fn -> putStrLn $ indent <> ", " <> show fn) (tail fns)
        putStrLn $ indent <> "]"

filenameSortKey :: FilePath -> Int
filenameSortKey fname =
    fromMaybe (error $ "can't parse " <> fname) (parse fname)
    where
    parse = Seq.head . mapMaybe Read.readMaybe . reverse . Seq.split "-"
        . FilePath.dropExtension

-- * StrokeMap

-- | Describe a drum-like instrument.  This is just the data for the various
-- functions to construct the patch.
data StrokeMap art = StrokeMap {
    -- | Map each articulation to the articulations that stop it.
    _stops :: Map art (Set art)
    , _strokes :: [Drums.Note]
    , _attributeMap :: Common.AttributeMap art
    } deriving (Show)

-- | Make a StrokeMap from a table with all the relevant info.
strokeMapTable :: Ord art => Drums.Stops
    -> [(Char, Expr.Symbol, Attrs.Attributes, art, Drums.Group)]
    -> StrokeMap art
strokeMapTable stops table = StrokeMap
    { _stops = stopMap [(art, group) | (_, _, _, art, group) <- table] stops
    , _strokes = map makeStroke table
    , _attributeMap = Common.attribute_map
        [(attrs, art) | (_, _, attrs, art, _) <- table]
    }
    where
    makeStroke (key, call, attrs, _, group) = Drums.Note
        { _name = call
        , _attributes = attrs
        , _char = key
        , _dynamic = 1
        , _group = group
        }

-- | Make a StrokeMap from separate strokes and AttributeMap.  This happens
-- when instruments parts are factored apart, due to having both MIDI and
-- im versions.
strokeMap :: Ord art => Drums.Stops -> [Drums.Note]
    -> Common.AttributeMap art -> StrokeMap art
strokeMap stops strokes attributeMap = StrokeMap
    { _stops = stopMap artToGroup stops
    , _strokes = strokes
    , _attributeMap = attributeMap
    }
    where
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
