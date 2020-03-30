-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | MMC is a MIDI standard for synchronizing with recording devices and DAWs
-- and whatever else chooses to support it.  Ultimately MMC is just a bunch of
-- specially formatted SystemExclusive msgs.
module Midi.Mmc where
import qualified Data.ByteString as B
import Data.Word (Word8)

import qualified Midi.Midi as Midi


-- | There are more, but I only support the ones I use.
data Mmc = Stop | Play | FastForward | Rewind | Pause
    | Goto !Midi.Smpte !SubFrames -- aka Locate
    deriving (Eq, Show)

-- TODO how many subframes per frame?
goto_seconds :: Midi.FrameRate -> Double -> Mmc
goto_seconds rate secs = Goto (Midi.seconds_to_smpte rate secs) 0

-- | This is just an arbitrary number that evidentally selects which device
-- should pay attention to the msg.  0x7f sometimes means all devices.
type DeviceId = Word8
type SubFrames = Word8

-- | Encode an Mmc msg into a SystemExclusive.
encode :: DeviceId -> Mmc -> Midi.Message
encode device_id msg =
    Midi.CommonMessage $ Midi.SystemExclusive 0x7f $
        B.pack [device_id, 0x06] <> encode_msg msg

encode_msg :: Mmc -> B.ByteString
encode_msg mmc = B.pack $ case mmc of
    Stop -> [0x1]
    Play -> [0x2]
    FastForward -> [0x4]
    Rewind -> [0x5]
    Pause -> [0x9]
    Goto (Midi.Smpte hours mins secs frames) subframes ->
        [0x44, 0x06, 0x01, hours, mins, secs, frames, subframes]
