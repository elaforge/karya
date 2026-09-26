-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | 'StaticConfig' is an app-level configuration hook.  The idea is that
-- the local configuration can use it to override things.
module App.StaticConfig (
    StaticConfig(..)
    , empty
    , getPostSetupCmd
    , Midi(..)
    , emptyMidi
    , makeRdevMap, makeWdevMap
    , makeReadDevices
    , cmdConfig
) where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.SyncKeycaps as SyncKeycaps

import qualified Derive.Derive as Derive
import qualified Instrument.Inst as Inst
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Color as Color

import           Global


-- * static config

data StaticConfig = StaticConfig
    { instrumentDb :: Cmd.InstrumentDb
    -- | These Cmds are run before any of the usual ones, and can implement
    -- local global keymaps and whatnot.
    , globalCmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]

    -- | Default global namespace for deriver calls.
    , builtins :: Derive.Builtins

    -- | Run this on startup, given the app's argv.  It can set up an initial
    -- block, load a given file, or do nothing.
    -- Remember that no block is focused when this is run, so cmds that need
    -- a focused block will abort.
    , setupCmd :: [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
        -- ^ Left on error
    , postSetupCmd :: Cmd.CmdT IO ()
    , midi :: Midi
    , highlightColors :: Map Color.Highlight Color.Color
    , imPlayDirect :: Bool -- ^ See 'Cmd.config_im_play_direct'.
    , keycaps :: Bool -- ^ Start with keycaps info window open.
    }

empty :: StaticConfig
empty = StaticConfig
    { instrumentDb = Inst.empty
    , globalCmds = []
    , builtins = mempty
    , setupCmd = const (Left "StaticConfig.setup_cmd not configured")
    , postSetupCmd = pure ()
    , midi = emptyMidi
    , highlightColors = mempty
    , imPlayDirect = False
    , keycaps = True
    }

getPostSetupCmd :: StaticConfig -> Cmd.CmdT IO ()
getPostSetupCmd config = do
    postSetupCmd config
    when (keycaps config) SyncKeycaps.open

data Midi = Midi
    -- | Reroute the hardware level read and write devices.  This way,
    -- instruments and saved scores can use symbolic names which are then
    -- mapped to the devices exported by the MIDI driver.
    --
    -- Because input devices are likely to be relatively static, the
    -- read device map is only configured here.
    { rdevMap :: Map Midi.ReadDevice Midi.ReadDevice
    -- | WriteDevices may vary per score, e.g. softsynths may listen at any
    -- number of virtual devices.  This map is taken as a default, but may
    -- be overridden by the score loaded.
    , wdevMap :: Map Midi.WriteDevice Midi.WriteDevice

    -- | Open the given devices on startup.  Even if they aren't plugged in,
    -- they'll be added to the read list so they're automatically connected if
    -- they are plugged in.  If you open a virtual device for both reading and
    -- writing you'll get a loop, so don't do that.
    --
    -- There's no corresponding write_devices because if you don't want
    -- to write to a device, just don't write to it!
    , readDevices :: Set Midi.ReadDevice
    -- | If set, and there are im instruments, there is an implicit automatic
    -- allocation for play-cache/ on this addr.
    , playCache :: Maybe (Midi.WriteDevice, Midi.Channel)
    } deriving (Show)

emptyMidi :: Midi
emptyMidi = Midi Map.empty Map.empty Set.empty Nothing

makeRdevMap :: [(Text, Text)] -> Map Midi.ReadDevice Midi.ReadDevice
makeRdevMap = Map.fromList . map (bimap Midi.readDevice Midi.readDevice)

makeWdevMap :: [(Text, Text)] -> Map Midi.WriteDevice Midi.WriteDevice
makeWdevMap = Map.fromList . map (bimap Midi.writeDevice Midi.writeDevice)

makeReadDevices :: [Text] -> Set Midi.ReadDevice
makeReadDevices = Set.fromList . map Midi.readDevice

-- | Create a 'Cmd.Config' from a StaticConfig.
cmdConfig :: Path.AppDir -> Path.Canonical -> Interface.Interface
    -> StaticConfig -> SaveGitT.User -> Cmd.Config
cmdConfig appDir saveDir interface config gitUser = Cmd.Config
    { configAppDir = appDir
    , configSaveDir = saveDir
    , configMidiInterface = interface
    , configKyPaths = map (Path.toAbsolute appDir) Config.kyPaths
    , configRdevMap = rdevMap midiConfig
    , configWdevMap = wdevMap midiConfig
    , configInstrumentDb = instrumentDb config
    , configBuiltins = builtins config
    , configHighlightColors = highlightColors config
    , configIm = Shared.Config.config appDir
    , configGitUser = gitUser
    , configImPlayDirect = imPlayDirect config
    , configPlayCache = playCache midiConfig
    }
    where midiConfig = midi config
