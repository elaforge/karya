-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Setup 'StaticConfig.StaticConfig'.  This is an empty example config, which
-- you'll want to customize locally.
module User.Generic.Config (loadStaticConfig) where
import qualified App.Config as Config
import qualified App.LoadInstruments as LoadInstruments
import qualified App.ParseArgs as ParseArgs
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Derive.C.All as C.All

import           Global


loadStaticConfig :: IO StaticConfig.StaticConfig
loadStaticConfig = do
    appDir <- Path.getAppDir
    instrumentDb <- LoadInstruments.load appDir
    midi <- getMidiConfig instrumentDb
    pure $ StaticConfig.StaticConfig
        { instrumentDb = instrumentDb
        , globalCmds = globalCmds
        , builtins = C.All.builtins
        , setupCmd = ParseArgs.parseArgs
        , postSetupCmd = pure ()
        , midi
        , highlightColors = Config.highlightColors
        -- Set to True to play im via portaudio, instead of the play_cache vst.
        , imPlayDirect = False
        , keycaps = True
        }

globalCmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
globalCmds = []

getMidiConfig :: Cmd.InstrumentDb -> IO StaticConfig.Midi
getMidiConfig _db = pure $ StaticConfig.Midi
    { rdevMap = StaticConfig.makeRdevMap []
    -- This assumes OS X, and IAC ports named 1, 2, 3, 4.
    , wdevMap = StaticConfig.makeWdevMap
        [("loop" <> showt n, "IAC Driver " <> showt n) | n <- [1..4]]
    , readDevices = mempty
    }
