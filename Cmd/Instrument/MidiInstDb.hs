-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.MidiInstDb (
    saveSynth, loadSynth
    , generateNames, cleanName
) where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time

import           System.FilePath ((</>))

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Logger as Logger

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Instrument.Serialize
import qualified Instrument.Tag as Tag

import qualified Perform.Midi.Patch as Patch

import           Global



-- | Some instruments want to load their patches in elaborate slow ways, like
-- parsing a directory full of sysexes.  These patches can export a @make_db@
-- function, which will do the slow parts and save the results in a cache file.
-- The @load@ function will simply read the cache file, if present.
saveSynth :: Path.AppDir -> InstT.SynthName -> [MidiInst.Patch] -> IO ()
saveSynth appDir synthName patches = do
    -- Assume these are loaded from files, so I'll need to generate valid
    -- names.
    let (patchMap, logs) = generateNames patches
    mapM_ (Log.notice . (("synth " <> synthName <> ": ") <>)) logs
    now <- Time.getCurrentTime
    Instrument.Serialize.serialize (dbPath appDir (untxt synthName)) $
        Instrument.Serialize.InstrumentDb now (stripCode <$> patchMap)
    where
    stripCode :: MidiInst.Patch -> (Patch.Patch, Common.Common ())
    stripCode (MidiInst.Patch patch _dummy common) =
        (patch, common { Common.commonCode = () })

loadSynth :: (Patch.Patch -> MidiInst.Code) -> InstT.SynthName -> Text
    -> Path.AppDir -> IO (Maybe MidiInst.Synth)
loadSynth getCode synthName doc appDir =
    Instrument.Serialize.unserialize fname >>= \case
        Left err -> do
            Log.warn $ "Error loading instrument db " <> showt fname <> ": "
                <> Text.strip (pretty err)
            pure Nothing
        Right (Instrument.Serialize.InstrumentDb _time patchMap) ->
            pure $ Just $ Inst.SynthDecl
                { synthdName = synthName
                , synthdDoc = doc
                , synthdPatches
                , synthdWarns
                }
            where
            (synthdPatches, synthdWarns) = MidiInst.extractWarns $
                map (second make) (Map.toList patchMap)
    where
    fname = dbPath appDir (untxt synthName)
    make (patch, common) = MidiInst.makeInst $
        (MidiInst.common #= common { Common.commonCode = getCode patch }) $
        MidiInst.makePatch patch

dbPath :: Path.AppDir -> FilePath -> FilePath
dbPath appDir name =
    Path.toAbsolute appDir Config.instrumentCacheDir </> name ++ ".db"

-- * generate_names

-- | 'Patch.inst_name' is the name as it appears on the synth, so it's not
-- guaranteed to be unique.  Also, due to loading from sysexes, there may be
-- duplicate patches.  Generate valid names for the patches, drop duplicates,
-- and disambiguate names that wind up the same.
generateNames :: [MidiInst.Patch] -> (Map InstT.Name MidiInst.Patch, [Text])
generateNames = -- This only touches the 'MidiInst.patch_patch' field.
    run . (concatMapM split <=< mapM dropDupInitialization)
        . Lists.keyedGroupSort (cleanName . instName)
    where
    run = first Map.fromList . Identity.runIdentity . Logger.run
    -- If the name and initialization is the same, they are likely duplicates.
    dropDupInitialization :: (InstT.Name, [MidiInst.Patch])
        -> Logger (InstT.Name, [MidiInst.Patch])
    dropDupInitialization (name, patches) = do
        let (unique, dups) = Lists.partitionDups
                (Patch.patchInitialize . MidiInst.patchPatch) patches
        forM_ dups $ \(patch, dups) ->
            log ("dropped patches with the same initialization as "
                <> details patch) dups
        pure (name, unique)
    -- The remaining patches are probably different and just happened to get
    -- the same name, so number them to disambiguate.
    split :: (InstT.Name, [MidiInst.Patch])
        -> Logger [(InstT.Name, MidiInst.Patch)]
    split (name, patches@(_:_:_)) = do
        let named = zip (map ((name<>) . showt) [1..]) patches
        log ("split into " <> Text.intercalate ", " (map fst named)) patches
        pure named
    split (name, patches) = pure $ map (name,) patches

    log _ [] = pure ()
    log msg patches = Logger.log $ msg <> ": "
        <> Text.intercalate ", " (map details patches)
    details patch =
        instName patch <> " (" <> fromMaybe "" (filename patch) <> ")"
    instName = Patch.patchName . MidiInst.patchPatch
    filename = lookup Tag.file . Common.commonTags . MidiInst.patchCommon

type Logger a = Logger.LoggerT Text Identity.Identity a

-- | People like to put wacky characters in their names, but it makes them
-- hard to type.
cleanName :: Text -> InstT.Name
cleanName =
    Text.dropWhileEnd (=='-') . Text.dropWhile (=='-')
        . stripDups
        . Text.filter (`elem` validInstrumentChars) . Text.map replace
        . Text.toLower
    where
    stripDups = Text.intercalate "-" . filter (not . Text.null)
        . Text.split (=='-')
    replace c
        | c `elem` (" _/" :: [Char]) = '-'
        | otherwise = c

validInstrumentChars :: [Char]
validInstrumentChars = '-' : ['0'..'9'] ++ ['a'..'z']
