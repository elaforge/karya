-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Setup 'StaticConfig.StaticConfig'.  This is an empty example config, which
-- you'll want to customize locally.
module User.Generic.Config (load_static_config) where
import qualified App.Config as Config
import qualified App.LoadInstruments as LoadInstruments
import qualified App.ParseArgs as ParseArgs
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.SyncKeycaps as SyncKeycaps

import qualified Derive.C.All as C.All

import           Global


load_static_config :: IO StaticConfig.StaticConfig
load_static_config = do
    app_dir <- Path.get_app_dir
    instrument_db <- LoadInstruments.load app_dir
    midi <- get_midi_config instrument_db
    return $ StaticConfig.StaticConfig
        { instrument_db = instrument_db
        , global_cmds = global_cmds
        , builtins = C.All.builtins
        , setup_cmd = ParseArgs.parse_args
        , post_setup_cmd = SyncKeycaps.open
        , midi = midi
        , highlight_colors = Config.highlight_colors
        -- Set to True to play im via portaudio, instead of the play_cache vst.
        , im_play_direct = False
        }

global_cmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
global_cmds = []

get_midi_config :: Cmd.InstrumentDb -> IO StaticConfig.Midi
get_midi_config _db = return $ StaticConfig.Midi
    { rdev_map = StaticConfig.make_rdev_map []
    -- This assumes OS X, and IAC ports named 1, 2, 3, 4.
    , wdev_map = StaticConfig.make_wdev_map
        [("loop" <> showt n, "IAC Driver " <> showt n) | n <- [1..4]]
    , read_devices = mempty
    }
