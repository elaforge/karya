-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Config.Hobbes where
import qualified App.StaticConfig as StaticConfig
import qualified Cmd.Cmd as Cmd
import qualified Midi.Midi as Midi

import           Global


midiConfig :: Cmd.InstrumentDb -> StaticConfig.Midi
midiConfig _db = StaticConfig.Midi
    { rdevMap = StaticConfig.makeRdevMap rdevMap
    , wdevMap = StaticConfig.makeWdevMap wdevMap
    , readDevices = StaticConfig.makeReadDevices readDevices
    , playCache = Just (Midi.writeDevice "loop4", 15)
    }

iac, tapco, network :: Int -> Text
iac n = "IAC Driver " <> showt n
tapco n = "Tapco Link MIDI USB Ver 2.2 Port " <> showt n
network n = "Network hobbes" <> showt n

wdevMap :: [(Text, Text)]
wdevMap = concat
    [ [("loop" <> showt n, iac n) | n <- [1..4]]
    , [("net" <> showt n, network n) | n <- [1..4]]
    , [ ("fm8", "Native Instruments FM8 Virtual Input")
      , ("msv", "Massive Virtual Input")
      , ("z1", tapco 1)
      , ("vl1", tapco 2)
      , ("morph", tapco 3)
      , ("pc2496", tapco 4)
      ]
    ]

rdevMap :: [(Text, Text)]
rdevMap =
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

-- | Open these read devices on startup.
readDevices :: [Text]
readDevices =
    [ "USB Oxygen 8 v2"
    , "EDIROL UA-25"
    , "828mk2 MIDI Port"
    ] ++ map tapco [1..4]
