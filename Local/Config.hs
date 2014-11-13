-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Setup StaticConfig.
module Local.Config (load_static_config) where
import qualified Control.Monad.Trans as Trans
import qualified Network.BSD

import qualified Util.Log as Log
import qualified Midi.Key as Key
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Controller as Controller
import qualified Cmd.Msg as Msg
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.Call.All as Call.All
import qualified Local.Config.Archy as Archy
import qualified Local.Config.Tammananny as Tammananny
import qualified Local.Instrument
import qualified Local.Setup as Setup

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig
import Global


load_static_config :: IO StaticConfig.StaticConfig
load_static_config = do
    app_dir <- Config.get_app_dir
    instrument_db <- Local.Instrument.load app_dir
    midi <- get_midi_config instrument_db
    return $ StaticConfig.StaticConfig {
        StaticConfig.instrument_db = instrument_db
        , StaticConfig.local_repl_dirs =
            [Config.make_path app_dir Config.repl_dir]
        , StaticConfig.global_cmds = global_cmds
        , StaticConfig.library = Call.All.library
        , StaticConfig.setup_cmd = parse_args
        , StaticConfig.midi = midi
        , StaticConfig.highlight_colors = Config.highlight_colors
        }

oxygen8_v2 :: Controller.TransportConfig
oxygen8_v2 = Controller.TransportConfig
    { Controller.config_repeat = note_on Key.cs_1
    , Controller.config_backward = note_on Key.ds_1
    , Controller.config_forward = note_on Key.fs_1
    , Controller.config_stop = note_on Key.gs_1
    , Controller.config_play = note_on Key.as_1
    , Controller.config_record = note_on Key.cs0
    }
    where note_on = Controller.note_on

global_cmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
global_cmds =
    [ Controller.transport oxygen8_v2
    ]

get_midi_config :: Cmd.InstrumentDb -> IO StaticConfig.Midi
get_midi_config db = Network.BSD.getHostName >>= \x -> case x of
    "tammananny" -> return $ Tammananny.midi_config db
    "archy" -> return $ Archy.midi_config db
    host -> do
      Log.warn $ "no midi configuration for host: " <> showt host
      return StaticConfig.empty_midi

parse_args :: [String] -> Cmd.CmdIO
parse_args argv = case argv of
    [] -> Save.load_template "save/default" >> return Cmd.Done
    -- Load a template.
    ["-t", fn] -> Save.load_template fn >> return Cmd.Done
    ["views", fn] -> Trans.liftIO (SaveGit.dump_views fn) >> return Cmd.Done
    ["mod", fn] -> Setup.load_mod fn
    ["midi", fn] -> Setup.load_midi fn
    [fn] -> Save.load fn >> return Cmd.Done
    [fn, ref_or_commit] -> do
        commit <- Cmd.require ("not a ref or commit: " ++ ref_or_commit)
            =<< Trans.liftIO (SaveGit.infer_commit fn ref_or_commit)
        Save.load_git fn (Just commit) >> return Cmd.Done
    _ -> error $ "bad args: " ++ show argv
