-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Setup 'StaticConfig.StaticConfig'.  This is an empty example config, which
-- you'll want to customize locally.
module User.Empty.Config (load_static_config) where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Derive.C.All as C.All
import qualified App.Config as Config
import qualified App.LoadInstruments as LoadInstruments
import qualified App.ParseArgs as ParseArgs
import qualified App.StaticConfig as StaticConfig


load_static_config :: IO StaticConfig.StaticConfig
load_static_config = do
    app_dir <- Config.get_app_dir
    instrument_db <- LoadInstruments.load app_dir
    midi <- get_midi_config instrument_db
    return $ StaticConfig.StaticConfig
        { instrument_db = instrument_db
        , global_cmds = global_cmds
        , builtins = C.All.builtins
        , setup_cmd = ParseArgs.parse_args
        , midi = midi
        , highlight_colors = Config.highlight_colors
        }

global_cmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
global_cmds = []

get_midi_config :: Cmd.InstrumentDb -> IO StaticConfig.Midi
get_midi_config _db = return StaticConfig.empty_midi
