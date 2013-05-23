module Local.Config.Archy where
import Util.Control
import qualified Cmd.Cmd as Cmd
import qualified Instrument.Db as Db
import qualified App.StaticConfig as StaticConfig


midi_config :: Cmd.InstrumentDb -> StaticConfig.Midi
midi_config db = StaticConfig.Midi
    { StaticConfig.rdev_map = StaticConfig.make_rdev_map rdev_map
    , StaticConfig.wdev_map = StaticConfig.make_wdev_map $
        -- Give all the softsynths a default mapping so they're easy to play
        -- with.
        [(dev, iac 1) | dev <- Db.synths db] ++ wdev_map
    , StaticConfig.read_devices = StaticConfig.make_read_devices read_devices
    }

iac, tapco, network :: Int -> Text
iac n = "IAC Synth " <> showt n
tapco n = "Tapco Port " <> showt n
network n = "Network archy" <> showt n

wdev_map :: [(Text, Text)]
wdev_map =
    [("loop" <> showt n, iac n) | n <- [1..4]] ++
    [("net" <> showt n, network n) | n <- [1..4]] ++
    [ ("fm8", "Native Instruments FM8 Virtual Input")
    , ("msv", "Massive Virtual Input")
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morph", tapco 3)
    , ("pc2496", tapco 4)
    ]

rdev_map :: [(Text, Text)]
rdev_map =
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

-- | Open these read devices on startup.
read_devices :: [Text]
read_devices =
    [ "Oxygen USB Oxygen 8 v2"
    , "EDIROL UA-25"
    , "828mk2 MIDI Port"
    ] ++ map tapco [1..4]
