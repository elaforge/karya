module Cmd.Instrument.MidiInstDb (
    save_synth, load_synth
    , generate_names, clean_name
) where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time

import           System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Maps as Maps
import qualified Util.Seq as Seq

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
save_synth :: Path.AppDir -> InstT.SynthName -> [MidiInst.Patch] -> IO ()
save_synth app_dir synth_name patches = do
    -- Assume these are loaded from files, so I'll need to generate valid
    -- names.
    let (patch_map, logs) = generate_names patches
    mapM_ (Log.notice . (("synth " <> synth_name <> ": ") <>)) logs
    now <- Time.getCurrentTime
    Instrument.Serialize.serialize (db_path app_dir (untxt synth_name)) $
        Instrument.Serialize.InstrumentDb now (strip_code <$> patch_map)
    where
    strip_code :: MidiInst.Patch -> (Patch.Patch, Common.Common ())
    strip_code (MidiInst.Patch patch common) =
        (patch, common { Common.common_code = () })

load_synth :: (Patch.Patch -> MidiInst.Code) -> InstT.SynthName -> Text
    -> Path.AppDir -> IO (Maybe MidiInst.Synth)
load_synth get_code synth_name doc app_dir = do
    let fname = db_path app_dir (untxt synth_name)
    Instrument.Serialize.unserialize fname >>= \case
        Left err -> do
            Log.warn $ "Error loading instrument db " <> showt fname <> ": "
                <> Text.strip (pretty err)
            return Nothing
        Right (Instrument.Serialize.InstrumentDb _time patch_map) ->
            return $ Just $ Inst.SynthDecl synth_name doc
                (map (second make) (Map.toList patch_map))
    where
    make (patch, common) = MidiInst.make_inst $ MidiInst.Patch patch $
        common { Common.common_code = get_code patch }

db_path :: Path.AppDir -> FilePath -> FilePath
db_path app_dir name =
    Path.to_absolute app_dir Config.instrument_cache_dir </> name ++ ".db"

-- * generate_names

-- | Like 'generate_names', but don't drop or rename duplicates, just report
-- them as errors.
check_names :: [MidiInst.Patch] -> (Map InstT.Name MidiInst.Patch, [InstT.Name])
check_names = second (map fst) . Maps.unique
    . Seq.key_on (Patch.patch_name . MidiInst.patch_patch)

-- | 'Patch.inst_name' is the name as it appears on the synth, so it's not
-- guaranteed to be unique.  Also, due to loading from sysexes, there may be
-- duplicate patches.  Generate valid names for the patches, drop duplicates,
-- and disambiguate names that wind up the same.
generate_names :: [MidiInst.Patch] -> (Map InstT.Name MidiInst.Patch, [Text])
generate_names = -- This only touches the 'MidiInst.patch_patch' field.
    run . (concatMapM split <=< mapM drop_dup_initialization)
        . Seq.keyed_group_sort (clean_name . inst_name)
    where
    run = first Map.fromList . Identity.runIdentity . Logger.run
    -- If the name and initialization is the same, they are likely duplicates.
    drop_dup_initialization :: (InstT.Name, [MidiInst.Patch])
        -> Logger (InstT.Name, [MidiInst.Patch])
    drop_dup_initialization (name, patches) = do
        let (unique, dups) = Seq.partition_dups
                (Patch.patch_initialize . MidiInst.patch_patch) patches
        forM_ dups $ \(patch, dups) ->
            log ("dropped patches with the same initialization as "
                <> details patch) dups
        return (name, unique)
    -- The remaining patches are probably different and just happened to get
    -- the same name, so number them to disambiguate.
    split :: (InstT.Name, [MidiInst.Patch])
        -> Logger [(InstT.Name, MidiInst.Patch)]
    split (name, patches@(_:_:_)) = do
        let named = zip (map ((name<>) . showt) [1..]) patches
        log ("split into " <> Text.intercalate ", " (map fst named)) patches
        return named
    split (name, patches) = return $ map (name,) patches

    log _ [] = return ()
    log msg patches = Logger.log $ msg <> ": "
        <> Text.intercalate ", " (map details patches)
    details patch =
        inst_name patch <> " (" <> fromMaybe "" (filename patch) <> ")"
    inst_name = Patch.patch_name . MidiInst.patch_patch
    filename = lookup Tag.file . Common.common_tags . MidiInst.patch_common

type Logger a = Logger.LoggerT Text Identity.Identity a

-- | People like to put wacky characters in their names, but it makes them
-- hard to type.
clean_name :: Text -> InstT.Name
clean_name =
    Text.dropWhileEnd (=='-') . Text.dropWhile (=='-')
        . strip_dups
        . Text.filter (`elem` valid_instrument_chars) . Text.map replace
        . Text.toLower
    where
    strip_dups = Text.intercalate "-" . filter (not . Text.null)
        . Text.split (=='-')
    replace c
        | c `elem` (" _/" :: [Char]) = '-'
        | otherwise = c

valid_instrument_chars :: [Char]
valid_instrument_chars = '-' : ['0'..'9'] ++ ['a'..'z']
