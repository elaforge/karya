module Local.Config.Archy where
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

iac, tapco, network :: Int -> String
iac n = "IAC Synth " ++ show n
tapco n = "Tapco Port " ++ show n
network n = "Network archy" ++ show n

wdev_map :: [(String, String)]
wdev_map =
    [("loop" ++ show n, iac n) | n <- [1..4]] ++
    [("net" ++ show n, network n) | n <- [1..4]] ++
    [ ("fm8", "Native Instruments FM8 Virtual Input")
    , ("msv", "Massive Virtual Input")
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morph", tapco 3)
    , ("pc2496", tapco 4)
    ]

rdev_map :: [(String, String)]
rdev_map =
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

-- | Open these read devices on startup.
read_devices :: [String]
read_devices =
    [ "Oxygen USB Oxygen 8 v2"
    , "EDIROL UA-25"
    , "828mk2 MIDI Port"
    ] ++ map tapco [1..4]
