-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.PatchDb (load_synth, normalize_patches) where
import qualified Control.Monad.Except as Except
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified Vivid.SC.SynthDef.Literally as Literally

import qualified Util.Exceptions as Exceptions
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Perform.Sc.Note as Note
import qualified Perform.Sc.Patch as Patch

import           Global


type PatchDb = Map Note.PatchName Patch.Patch

c_gate :: ScoreT.Control
c_gate = ScoreT.Control "gate"

-- * load

load_synth :: Path.AppDir -> IO (Maybe (Inst.SynthDecl Cmd.InstrumentCode))
load_synth app_dir = fmap synth <$> load app_dir

synth :: PatchDb -> Inst.SynthDecl Cmd.InstrumentCode
synth patches = Inst.SynthDecl "sc" "supercollider" $
    map (bimap Texts.toText make) (Map.toList patches)
    where
    make patch = Inst.Inst
        { inst_backend = Inst.Sc patch
        , inst_common = Common.common Cmd.empty_code
        }

load :: Path.AppDir -> IO (Maybe PatchDb)
load app_dir = load_dir (Path.to_absolute app_dir Config.sc_dir)

load_dir :: FilePath -> IO (Maybe PatchDb)
load_dir dir = Exceptions.ignoreEnoent (File.list dir) >>= \case
    Nothing -> do
        Log.notice $ "no supercollider patch dir: " <> showt dir
        return Nothing
    Just fnames -> do
        (errors, patches) <- Either.partitionEithers <$> mapM load_file fnames
        mapM_ Log.warn errors
        -- If you ran normalize_patches, there should be no collisions, since
        -- the filesystem disallows them.
        return $ Just $ Map.unions patches

load_file :: FilePath -> IO (Either Text PatchDb)
load_file fname =
    fmap (first (txt . (prefix<>)) . parse fname) $ ByteString.readFile fname
    where prefix = fname <> ": "

parse :: FilePath -> ByteString.ByteString -> Either String PatchDb
parse fname bytes = do
    Literally.SynthDefFile defs <- Literally.decodeSynthDefFile bytes
    Map.fromList . zip (map Literally._synthDefName defs) <$>
        mapM (convert fname) defs

convert :: FilePath -> Literally.LiteralSynthDef -> Either String Patch.Patch
convert fname def = do
    gate_id <- maybe (Left $ "no 'gate' control: " <> prettys controls) return $
        Map.lookup c_gate controls
    unless (gate_id == Note.gate_id) $
        Left $ "gate control ID should be " <> prettys Note.gate_id <> ", was "
            <> prettys gate_id
    return $ Patch.Patch
        { name = Literally._synthDefName def
        , filename = fname
        , controls = Map.delete c_gate controls
        }
    where
    controls = Map.fromList
        [ (ScoreT.unchecked_control (Texts.toText name), Note.ControlId ix)
        | Literally.ParamName name ix <- Literally._synthDefParamNames def
        ]


-- * normalize

-- | Process a directory of .scsyndef patches to validate and make names
-- consistent with filenames.  Call from ghci.
normalize_patches :: FilePath -> FilePath -> IO ()
normalize_patches in_dir out_dir = do
    fnames <- File.list in_dir
    forM_ fnames $ \fname -> normalize_patch out_dir fname >>= \case
        Left err -> putStrLn $ fname <> ": " <> err
        Right () -> return ()

normalize_patch :: FilePath -> FilePath -> IO (Either String ())
normalize_patch out_dir fname = Except.runExceptT $ do
    Literally.SynthDefFile defs <- tryRight . Literally.decodeSynthDefFile
        =<< liftIO (ByteString.readFile fname)
    case defs of
        [def] -> fix "" def
        defs -> mapM_ (uncurry fix) $ zip (map show [1..]) defs
    where
    fix suffix def = do
        _ <- tryRight $ convert fname def
        let old_name = Literally._synthDefName def
        let fixed = def { Literally._synthDefName = Texts.toByteString name }
        when (old_name /= Texts.toByteString name) $
            put $ "renamed " <> show old_name <> " -> " <> show name
        let out_fname = out_dir </> name <> suffix <> ".scsyndef"
        put $ fname <> " written to " <> out_fname
        liftIO $ ByteString.writeFile out_fname
            (Literally.encodeLiteralSynthDef fixed)
    name = fst $ Lists.dropSuffix ".scsyndef" $ FilePath.takeFileName fname
    put = liftIO . putStrLn
