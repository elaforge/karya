-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.PatchDb (loadSynth, normalizePatches) where
import qualified Control.Monad.Except as Except
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified Vivid.SC.SynthDef.Literally as Literally

import qualified Util.Exceptions as Exceptions
import qualified Util.Files as Files
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

cGate :: ScoreT.Control
cGate = ScoreT.Control "gate"

-- * load

loadSynth :: Path.AppDir -> IO (Maybe (Inst.SynthDecl Cmd.InstrumentCode))
loadSynth appDir = fmap synth <$> load appDir

synth :: PatchDb -> Inst.SynthDecl Cmd.InstrumentCode
synth patches = Inst.SynthDecl
    { synthdName = "sc"
    , synthdDoc = "supercollider"
    , synthdPatches = map (bimap Texts.toText make) (Map.toList patches)
    , synthdWarns = []
    }
    where
    make patch = Inst.Inst
        { instBackend = Inst.Sc patch
        , instCommon = Common.common Cmd.emptyCode
        }

load :: Path.AppDir -> IO (Maybe PatchDb)
load appDir = loadDir (Path.toAbsolute appDir Config.scDir)

loadDir :: FilePath -> IO (Maybe PatchDb)
loadDir dir = Exceptions.ignoreEnoent (Files.list dir) >>= \case
    Nothing -> do
        Log.notice $ "no supercollider patch dir: " <> showt dir
        pure Nothing
    Just fnames -> do
        (errors, patches) <- Either.partitionEithers <$> mapM loadFile fnames
        mapM_ Log.warn errors
        -- If you ran normalize_patches, there should be no collisions, since
        -- the filesystem disallows them.
        pure $ Just $ Map.unions patches

loadFile :: FilePath -> IO (Either Text PatchDb)
loadFile fname =
    fmap (first (txt . (prefix<>)) . parse fname) $ ByteString.readFile fname
    where prefix = fname <> ": "

parse :: FilePath -> ByteString.ByteString -> Either String PatchDb
parse fname bytes = do
    Literally.SynthDefFile defs <- Literally.decodeSynthDefFile bytes
    Map.fromList . zip (map Literally._synthDefName defs) <$>
        mapM (convert fname) defs

convert :: FilePath -> Literally.LiteralSynthDef -> Either String Patch.Patch
convert fname def = do
    gateId <- maybe (Left $ "no 'gate' control: " <> prettys controls) pure $
        Map.lookup cGate controls
    unless (gateId == Note.gateId) $
        Left $ "gate control ID should be " <> prettys Note.gateId <> ", was "
            <> prettys gateId
    pure $ Patch.Patch
        { name = Literally._synthDefName def
        , filename = fname
        , controls = Map.delete cGate controls
        }
    where
    controls = Map.fromList
        [ (ScoreT.Control (Texts.toText name), Note.ControlId ix)
        | Literally.ParamName name ix <- Literally._synthDefParamNames def
        ]


-- * normalize

-- | Process a directory of .scsyndef patches to validate and make names
-- consistent with filenames.  Call from ghci.
normalizePatches :: FilePath -> FilePath -> IO ()
normalizePatches inDir outDir = do
    fnames <- Files.list inDir
    forM_ fnames $ \fname -> normalizePatch outDir fname >>= \case
        Left err -> putStrLn $ fname <> ": " <> err
        Right () -> pure ()

normalizePatch :: FilePath -> FilePath -> IO (Either String ())
normalizePatch outDir fname = Except.runExceptT $ do
    Literally.SynthDefFile defs <- tryRight . Literally.decodeSynthDefFile
        =<< liftIO (ByteString.readFile fname)
    case defs of
        [def] -> fix "" def
        defs -> mapM_ (uncurry fix) $ zip (map show [1..]) defs
    where
    fix suffix def = do
        _ <- tryRight $ convert fname def
        let oldName = Literally._synthDefName def
        let fixed = def { Literally._synthDefName = Texts.toByteString name }
        when (oldName /= Texts.toByteString name) $
            put $ "renamed " <> show oldName <> " -> " <> show name
        let outFname = outDir </> name <> suffix <> ".scsyndef"
        put $ fname <> " written to " <> outFname
        liftIO $ ByteString.writeFile outFname
            (Literally.encodeLiteralSynthDef fixed)
    name = fst $ Lists.dropSuffix ".scsyndef" $ FilePath.takeFileName fname
    put = liftIO . putStrLn
