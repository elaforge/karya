-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Setup StaticConfig.
module User.Elaforge.Config (loadStaticConfig) where
import qualified Data.Map as Map
import qualified System.FilePath as FilePath

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Network as Network

import qualified App.Config as Config
import qualified App.LoadInstruments as LoadInstruments
import qualified App.ParseArgs as ParseArgs
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Controller as Controller
import qualified Cmd.Load.Med as Load.Med
import qualified Cmd.Load.Mod as Load.Mod
import qualified Cmd.Load.ModSexpr as ModSexpr
import qualified Cmd.Load.ModT as ModT
import qualified Cmd.Msg as Msg

import qualified Derive.C.All as C.All
import qualified Derive.ScoreT as ScoreT
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Pitch as Pitch
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified User.Elaforge.Config.Hobbes as Hobbes
import qualified User.Elaforge.Config.Tammananny as Tammananny

import           Global


keycaps :: Bool
keycaps = False

imPlayDirect :: Bool
imPlayDirect = False

loadStaticConfig :: IO StaticConfig.StaticConfig
loadStaticConfig = do
    appDir <- Path.getAppDir
    instrumentDb <- LoadInstruments.load appDir
    midi <- getMidiConfig instrumentDb
    pure $ StaticConfig.StaticConfig
        { instrumentDb
        , globalCmds
        , builtins = C.All.builtins
        , setupCmd = parseArgs
        , postSetupCmd = pure ()
        , midi
        , highlightColors = Config.highlightColors
        , imPlayDirect
        , keycaps
        }

parseArgs :: [String] -> Either Text (Cmd.CmdT IO Cmd.Status)
parseArgs = \case
    ["med", fname] -> Right $ loadMed fname
    ["mod-sexpr", fname] -> Right $ loadModSexpr fname
    args -> ParseArgs.parseArgs args

oxygen8V2 :: Controller.TransportConfig
oxygen8V2 = Controller.TransportConfig
    { configRepeat = noteOn Key.cs_1
    , configBackward = noteOn Key.ds_1
    , configForward = noteOn Key.fs_1
    , configStop = noteOn Key.gs_1
    , configPlay = noteOn Key.as_1
    , configRecord = noteOn Key.cs0
    }
    where noteOn = Controller.noteOn

globalCmds :: [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
globalCmds =
    [ Controller.transport oxygen8V2
    ]

getMidiConfig :: Cmd.InstrumentDb -> IO StaticConfig.Midi
getMidiConfig db = do
    fullHost <- Network.getHostName
    case takeWhile (/='.') fullHost of
        "tammananny" -> pure $ Tammananny.midiConfig db
        "hobbes" -> pure $ Hobbes.midiConfig db
        "groqmac" -> pure $ Hobbes.midiConfig db
        host -> do
          Log.warn $ "no midi configuration for host: " <> showt host
          pure defaultMidi

defaultMidi :: StaticConfig.Midi
defaultMidi = StaticConfig.Midi
    { rdevMap = mempty
    , wdevMap = StaticConfig.makeWdevMap
        [("loop" <> showt n, iac n) | n <- [1..4]]
    , readDevices = mempty
    , playCache = Just (Midi.writeDevice "loop4", 15)
    }
    where iac n = "IAC Driver " <> showt n

-- * mod

loadModSexpr :: FilePath -> Cmd.CmdT IO Cmd.Status
loadModSexpr fname =
    convertMod fname =<< Cmd.requireRight id =<< liftIO (ModSexpr.load fname)

loadMed :: FilePath -> Cmd.CmdT IO Cmd.Status
loadMed fname = convertMod fname =<< liftIO (Load.Med.load fname)

convertMod :: Cmd.M m => FilePath -> ModT.Module -> m Cmd.Status
convertMod fname mod = do
    state <- Cmd.requireRight pretty $ Load.Mod.convert (fnToNs fname) $
        ModT.transposeInstruments transpose $
        ModT.mapInstruments instMap mod
    Ui.put state
    pure Cmd.Done
    where
    instMap = Map.findWithDefault mempty (FilePath.takeFileName fname)
        instMaps
    transpose = Map.findWithDefault mempty (FilePath.takeFileName fname)
        transposeMaps

fnToNs :: FilePath -> Id.Namespace
fnToNs = Id.namespace . txt . fromMaybe (error "postcondition") . Lists.head
    . Lists.split (=='.') . FilePath.takeFileName

instMaps :: Map FilePath (Map Text Text)
instMaps = Map.fromList
    [ ("underwater", Map.fromList
        [ ("Takerimba", "marim")
        , ("SoftShake", "shake")
        , ("Thumb Bass", "bass")
        , ("HeavyBassDrum", "bd")
        , ("SD1", "sd")
        , ("FireHiSyn", "lead")
        , ("VCO Bass", "synb")
        , ("Chin-PanFluteLooped", "pan")
        , ("WoodPf (4/29)", "wood")
        , ("RainyHiMajor", "maj")
        , ("RainyHiMinor", "min")
        , ("technoRush-loud", "rush1")
        , ("technoRush2", "rush2")
        , ("D50-PizzaGogo", "pizz")
        , ("filter.maj", "fmaj")
        ])
    , ("piano", Map.fromList
        [ ("UpPiano (4/1)", "pno")
        , ("UprtBass (6/20)", "bass")
        , ("Glockn2 (6/36)", "glock")
        , ("BigPipe (7/13)", "pipe")
        , ("String2 (5/17)", "string")
        , ("TubeBe1 (6/37)", "bell")
        ])
    , ("Elektrodes", Map.fromList
        [ ("Elektrodes", "elec")
        , ("Jazz Man", "bass")
        , ("440thick-bk", "bd")
        , ("AquaSnare", "sn")
        , ("AlesisHihatC", "hh-c")
        , ("AlesisHihatO", "hh-o")
        , ("AlesisHihatM", "hh-m")
        , ("CheckHiSyn-loud", "syn")
        , ("ClassPiano", "pno")
        , ("BstTom", "tom")
        , ("SundanceJazzHit", "hit")
        ])
    , ("green-mold.sexp",) $ Map.fromList
        [ ("chip 1 -", "c1")
        , ("chip 2 --", "c2")
        , ("chip 3 ---", "c3")
        , ("b 1 -", "b1")
        , ("b 2 --", "b2")
        , ("b 3 ---", "b3")
        ]
    ]

transposeMaps :: Map FilePath (Map ScoreT.Instrument Pitch.NoteNumber)
transposeMaps = Map.fromList
    [ ("green-mold.sexp",) $ Map.fromList
        [ ("b1", -12*3)
        , ("b2", -12*3)
        , ("b3", -12*3)
        ]
    ]
