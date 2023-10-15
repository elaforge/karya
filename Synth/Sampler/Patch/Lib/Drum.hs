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
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Data.Typeable as Typeable

import qualified GHC.Stack as Stack
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Num as Num

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.ImInst as ImInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr

import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Code as Code
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


-- * patch

-- | Make a complete sampler patch with all the drum bits.
patch :: Ord art => FilePath -> Note.PatchName -> StrokeMap art
    -> ConvertMap art -> (Maybe art -> CUtil.CallConfig) -> Patch.Patch
patch dir name strokeMap convertMap configOf = (Patch.patch name)
    { Patch._dir = dir
    , Patch._preprocess =
        if Map.null (_stops strokeMap) then id else inferDuration strokeMap
    , Patch._convert = convert (_attributeMap strokeMap) convertMap
    , Patch._karyaPatch = karyaPatch dir strokeMap convertMap configOf
        (_extraCalls strokeMap)
    , Patch._allFilenames = _allFilenames convertMap
    }

-- | Make a patch with the drum-oriented code in there already.
karyaPatch :: FilePath -> StrokeMap art -> ConvertMap art
    -> (Maybe art -> CUtil.CallConfig)
    -> [(Maybe Char, Expr.Symbol, Derive.Generator Derive.Note)]
    -> ImInst.Patch
karyaPatch dir strokeMap convertMap configOf extraCalls =
    CUtil.im_drum_patch (map fst (_strokes strokeMap)) $
    ImInst.code #= code $
    makePatch (_attributeMap strokeMap) (Maybe.isJust (_naturalNn convertMap))
    where
    extraCmds = [(char, sym) | (Just char, sym, _) <- extraCalls]
    code = CUtil.drum_code_cmd extraCmds thru
        [ (stroke, set (configOf mbArt))
        | (stroke, mbArt) <- _strokes strokeMap
        ]
        <> ImInst.note_generators [(sym, call) | (_, sym, call) <- extraCalls]
    set config = config { CUtil._transform = Code.withVariation }
    thru = Util.imThruFunction dir
        (convert (_attributeMap strokeMap) convertMap)

-- | Make an unconfigured patch, without code, in case it's too custom for
-- 'karyaPatch'.
makePatch :: Common.AttributeMap a -> Bool -> ImInst.Patch
makePatch attributeMap hasNaturalNn = ImInst.make_patch $ Im.Patch.patch
    { Im.Patch.patch_controls = mconcat
        [ Control.supportDyn
        , Control.supportVariation
        , if hasNaturalNn then Control.supportPitch else mempty
        ]
    , Im.Patch.patch_attribute_map = const () <$> attributeMap
    }

-- * convert

-- | Arguments for the 'convert' function.
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
    -- | articulation -> dynamic -> variation -> (FilePath, (lowDyn, highDyn)).
    -- Returning the sample's dyn range was an attempt to tune dyn globally,
    -- but I think it doesn't work, see TODO above.
    , _getFilename :: art -> Util.Dyn -> Signal.Y
        -> (Maybe FilePath, Maybe (Signal.Y, Signal.Y))
    , _allFilenames :: Set FilePath
    }

-- | Create a '_getFilename' with the strategy where each articulation has
-- a @[FilePath]@, sorted evenly over the dynamic range.
variableDynamic :: Show art => Signal.Y
    -- ^ A note may pick a sample of this much dyn difference on either side.
    -> (art -> [FilePath])
    -> (art -> Util.Dyn -> Signal.Y -> (Maybe FilePath, Maybe a))
    -- ^ Maybe is unused, it's for compatibility with '_getFilename'
variableDynamic variationRange articulationSamples = \art dyn var ->
    ( (show art </>) <$>
        pickDynamicVariation variationRange (articulationSamples art) dyn var
    , Nothing
    )

-- | '_allFilenames' for ConvertMaps that use 'variableDynamic' and
-- 'makeFileList'.
allFilenames :: (Stack.HasCallStack, Enum a, Bounded a, Show a)
    => Int -> (a -> [FilePath]) -> Set FilePath
allFilenames len articulationSamples = Util.assertLength len $ Set.fromList
    [ show art </> fname
    | art <- Util.enumAll
    , fname <- articulationSamples art
    ]

-- | Make a generic convert, suitable for drum type patches.
convert :: Common.AttributeMap art -> ConvertMap art -> Note.Note
    -> Patch.ConvertM Sample.Sample
convert attributeMap cmap = \note -> do
    articulation <- Util.articulation attributeMap (Note.attributes note)
    let dyn = Note.initial0 Control.dynamic note
    let var = fromMaybe 0 $ Note.initial Control.variation note
    let (mbFilename, mbDynRange) = getFilename articulation dyn var
    filename <- tryJust "no sample" mbFilename
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
    where
    ConvertMap (minDyn, maxDyn) naturalNn muteTime getFilename _allFilenames =
        cmap

-- * StrokeMap

-- | Describe a drum-like instrument.  This is just the data for the various
-- functions to construct the patch.
data StrokeMap art = StrokeMap {
    -- | Map each articulation to the articulations that stop it.
    _stops :: Map art (Set art)
    -- | Retain the Stroke to 'art' assocation so I can have it when generating
    -- the call for each stroke.
    , _strokes :: [(Drums.Stroke, Maybe art)]
    , _attributeMap :: Common.AttributeMap art
    , _extraCalls :: [(Maybe Char, Expr.Symbol, Derive.Generator Derive.Note)]
    } deriving (Show)

-- | Like 'strokeMapTable', but for patches with only 'Stroke's.
strokeMapSimple :: Ord art => Drums.Stops
    -> [(Char, Expr.Symbol, Attrs.Attributes, art, Drums.Group)]
    -> StrokeMap art
strokeMapSimple stops table = strokeMapTable stops
    [ (key, sym, Stroke attrs art group)
    | (key, sym, attrs, art, group) <- table
    ]

data Call art =
    -- | A call that doesn't correspond directly to Attributes.
    Call (Derive.Generator Derive.Note)
    -- | Emit a call that produces these Attributes, and cause the patch to
    -- assign those Attributes to the given articulation.
    | Stroke Attrs.Attributes art Drums.Group
    -- | Emit a call that produces these Attributes, with no articulation
    -- association.
    | Attr Attrs.Attributes

-- | Make a StrokeMap describing the keymap and call map of a drum-like patch.
strokeMapTable :: Ord art => Drums.Stops
    -> [(Char, Expr.Symbol, Call art)]
    -- ^ If Char == ' ', there is no key binding.  Symbol == "" means there
    -- is no call, but it still makes sense for 'Stroke', because it can make
    -- the patch respond to the given Attributes.
    -> StrokeMap art
strokeMapTable stops table = StrokeMap
    { _stops =
        stopMap [(art, group) | (_, _, Stroke _ art group) <- table] stops
    , _strokes = strokes
    , _attributeMap = Common.attribute_map $
        [ (attrs, art)
        | (_, _, call) <- table
        , Just attrs <- [attrsOf call], Just art <- [artOf call]
        ]
    , _extraCalls =
        [ (if char == ' ' then Nothing else Just char, sym, call)
        | (char, sym, Call call) <- table
        ]
    }
    where
    strokes =
        [ (makeStroke key sym attrs, artOf call)
        | (key, sym, call) <- table
        , Just attrs <- [attrsOf call]
        , key /= ' '
        ]
    attrsOf (Call {}) = Nothing
    attrsOf (Stroke attrs _ _) = Just attrs
    attrsOf (Attr attrs) = Just attrs
    artOf (Stroke _ art _) = Just art
    artOf _ = Nothing
    makeStroke key sym attrs = Drums.Stroke
        { _name = sym
        , _attributes = attrs
        , _char = key
        , _dynamic = 1
        -- Drums._group is for generating 'stopMap' from just Strokes, but
        -- I'm generating it separately here.
        , _group = ""
        }

addAttributeMap :: Common.AttributeMap art -> StrokeMap art -> StrokeMap art
addAttributeMap attrs strokeMap =
    strokeMap { _attributeMap = attrs <> _attributeMap strokeMap }

-- | Set dynamic for Attrs.soft and remove it.
replaceSoft :: Signal.Y -> StrokeMap art -> StrokeMap art
replaceSoft dyn strokeMap =
    strokeMap { _strokes = map (first replace) (_strokes strokeMap) }
    where
    replace stroke = stroke
        { Drums._attributes = Attrs.remove Attrs.soft attrs
        , Drums._dynamic = if Attrs.contain attrs Attrs.soft then dyn else 1
        }
        where attrs = Drums._attributes stroke

-- | Make a StrokeMap from separate strokes and AttributeMap.  This happens
-- when instruments parts are factored apart, due to having both MIDI and
-- im versions.
strokeMap :: Ord art => Drums.Stops -> [Drums.Stroke]
    -> Common.AttributeMap art -> StrokeMap art
strokeMap stops strokes attributeMap = StrokeMap
    { _stops = stopMap artToGroup stops
    , _strokes = zip strokes (map strokeToArt strokes)
    , _attributeMap = attributeMap
    , _extraCalls = []
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

type Articulation = FilePath

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
        fns <- Lists.sortOn filenameSortKey <$>
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
    parse = Lists.head . mapMaybe Read.readMaybe . reverse . Lists.split "-"
        . FilePath.dropExtension

-- | Emit haskell code for a function from an Enum to lists.
enumFunction
    :: (Typeable.Typeable a, Show a, Typeable.Typeable b, Pretty b)
    => String -> [(a, [b])] -> [String]
enumFunction name abs@((a0, b0) : _) =
    [ name <> " :: " <> show (Typeable.typeOf a0)
        <> " -> " <> show (Typeable.typeOf b0)
    , name <> " = \\case"
    ] ++ concatMap (indent . makeCase) abs
    where
    makeCase (a, bs) = show a <> " ->"
        : indent (Lists.mapHeadTail ("[ "<>) (", "<>) (map prettys bs) ++ ["]"])
    indent = map (replicate 4 ' ' <>)
enumFunction name _ = error $ "function has no values: " <> name
