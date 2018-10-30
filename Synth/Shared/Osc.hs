-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Shared.Osc (ThruFunction, Play(..), send, play, stop) where
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Sound.OSC as OSC
import qualified Sound.OSC.Transport.FD as OSC.Transport.FD
import qualified Sound.OSC.Transport.FD.UDP as OSC.Transport.FD.UDP

import qualified Derive.Attrs as Attrs
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Config as Config

import Global


-- | This is a specialized version of 'Cmd.Cmd.ThruFunction'.  Being more
-- specialized means I don't have to directly depend on "Cmd.Cmd" from here.
type ThruFunction = Attrs.Attributes -> Pitch.NoteNumber -> Double -- velocity
    -> Either Error [Play]

type Error = Text

data Play = Play {
    _sample :: !FilePath
    , _ratio :: !Double
    , _volume :: !Double
    } deriving (Eq, Show)

send :: OSC.Message -> IO ()
send msg =
    OSC.Transport.FD.withTransport open $ \osc ->
        OSC.Transport.FD.sendMessage osc msg
    where
    open = OSC.Transport.FD.UDP.openUDP "127.0.0.1" Config.oscPort

play :: Play -> OSC.Message
play (Play fname ratio volume) = OSC.message "/play"
    [ OSC.ASCII_String (ByteString.Char8.pack fname)
    , OSC.Double ratio, OSC.Double volume
    ]

stop :: OSC.Message
stop :: OSC.Message = OSC.message "/stop" []




start = send $ play $ Play
    "/Users/elaforge/src/seq/data/sampler/reyong/62-109-127-open+v1.flac" 1 1
stop1 = send stop
