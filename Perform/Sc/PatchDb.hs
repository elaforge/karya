-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Sc.PatchDb (load_synth) where
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Vivid.SC.SynthDef.Literally as Literally

import qualified Util.File as File
import qualified Util.Log as Log
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

c_duration :: ScoreT.Control
c_duration = ScoreT.Control "duration"

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
load_dir dir = File.ignoreEnoent (File.list dir) >>= \case
    Nothing -> do
        Log.notice $ "no supercollider patch dir: " <> showt dir
        return Nothing
    Just fnames -> do
        (errors, patches) <- Either.partitionEithers <$> mapM parse_file fnames
        mapM_ Log.warn errors
        -- TODO warn about collisions
        return $ Just $ Map.unions patches

parse_file :: FilePath -> IO (Either Text PatchDb)
parse_file fname =
    fmap (first (txt . (prefix<>)) . parse) $ ByteString.readFile fname
    where prefix = fname <> ": "

parse :: ByteString.ByteString -> Either String PatchDb
parse bytes = do
    Literally.SynthDefFile defs <- Literally.decodeSynthDefFile bytes
    Map.fromList . zip (map Literally._synthDefName defs) <$> mapM convert defs
    where
    convert def = do
        dur_id <- maybe (Left "no 'duration' control") return $
            Map.lookup c_duration controls
        return $ Patch.Patch
            { name = Literally._synthDefName def
            , duration_control = dur_id
            , controls = Map.delete c_duration controls
            }
        where
        controls = Map.fromList
            [ (ScoreT.unchecked_control (Texts.toText name), Note.ControlId ix)
            | Literally.ParamName name ix <- Literally._synthDefParamNames def
            ]
