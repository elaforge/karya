-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Config.Tammananny where
import qualified App.StaticConfig as StaticConfig
import qualified Cmd.Cmd as Cmd

import           Global


midiConfig :: Cmd.InstrumentDb -> StaticConfig.Midi
midiConfig _db = StaticConfig.Midi
    { rdevMap = StaticConfig.makeRdevMap rdevMap
    , wdevMap = StaticConfig.makeWdevMap wdevMap
    , readDevices = StaticConfig.makeReadDevices readDevices
    , playCache = Nothing
    }

-- | jack1 copies the port name over from ALSA and then truncates it.
-- jack2 just hardcodes to midi_capture_n, which is pretty useless since it
-- depends what order you plugged the things in.
alsaInput :: Text -> Int -> Int -> Text
alsaInput name device sub =
    "in-hw-" <> showt device <> "-0-" <> showt sub <> "-" <> name

alsaOutput :: Text -> Int -> Int -> Text
alsaOutput name device sub =
    "out-hw-" <> showt device <> "-0-" <> showt sub <> "-" <> name

tapcoName :: Text
tapcoName = "Tapco-Link-MIDI-USB-Ver-2-2-MID"

tapcoIn, tapcoOut :: Int -> Text
tapcoIn n = alsaInput tapcoName 3 (n-1)
tapcoOut n = alsaOutput tapcoName 3 (n-1)

wdevMap :: [(Text, Text)]
wdevMap =
    [ ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morph", tapco 2)
    , ("pc2496", tapco 3)
    , ("capybara", tapco 4)
    ]
    where tapco = tapcoOut

rdevMap :: [(Text, Text)]
rdevMap =
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]
    where tapco = tapcoIn

readDevices :: [Text]
readDevices = alsaInput "USB-Oxygen-8-v2-MIDI-1" 2 0 : map tapco [1..4]
    where tapco = tapcoIn
