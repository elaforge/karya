module Local.Config.Tammananny where
import qualified Cmd.Cmd as Cmd
import qualified App.StaticConfig as StaticConfig


midi_config :: Cmd.InstrumentDb -> StaticConfig.Midi
midi_config _db = StaticConfig.Midi
    { StaticConfig.rdev_map = StaticConfig.make_rdev_map rdev_map
    , StaticConfig.wdev_map = StaticConfig.make_wdev_map wdev_map
    , StaticConfig.read_devices = StaticConfig.make_read_devices read_devices
    }

-- | jack1 copies the port name over from ALSA and then truncates it.
-- jack2 just hardcodes to midi_capture_n, which is pretty useless since it
-- depends what order you plugged the things in.
alsa_input :: String -> Int -> Int -> String
alsa_input name device sub =
    "in-hw-" ++ show device ++ "-0-" ++ show sub ++ "-" ++ name

alsa_output :: String -> Int -> Int -> String
alsa_output name device sub =
    "out-hw-" ++ show device ++ "-0-" ++ show sub ++ "-" ++ name

tapco_name :: String
tapco_name = "Tapco-Link-MIDI-USB-Ver-2-2-MID"

tapco_in, tapco_out :: Int -> String
tapco_in n = alsa_input tapco_name 3 (n-1)
tapco_out n = alsa_output tapco_name 3 (n-1)

wdev_map :: [(String, String)]
wdev_map =
    [ ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morph", tapco 2)
    , ("pc2496", tapco 3)
    , ("capybara", tapco 4)
    ]
    where tapco = tapco_out

rdev_map :: [(String, String)]
rdev_map =
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]
    where tapco = tapco_in

read_devices :: [String]
read_devices =
    [ alsa_input "USB-Oxygen-8-v2-MIDI-1" 2 0
    ] ++ map tapco [1..4]
    where tapco = tapco_in
