-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Fire up the play-cache vst.
module Perform.Im.Play (
    play_cache_synth, qualified
    , prepare, midi_to_offset, start, stop
) where
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global
import Types


play_cache_synth :: Inst.SynthDecl Cmd.InstrumentCode
play_cache_synth = Inst.SynthDecl synth_name
    "play_cache VST, to play the output of offline synthesizers."
    [(Patch.default_name, inst)]
    where
    inst = Inst.Inst
        { inst_backend = Inst.Midi $ Patch.patch (0, 0) Patch.default_name
        , inst_common = Common.doc #= doc $ Common.common Cmd.empty_code
        }
    doc = "This dummy patch is just to give a way to configure the MIDI\
        \ channel of the play_cache VST. It's never actually played, and the\
        \ instrument should never be used in the score."

qualified :: InstTypes.Qualified
qualified = InstTypes.Qualified synth_name Patch.default_name

synth_name :: InstTypes.SynthName
synth_name = "play-cache"

sampling_rate :: Double
sampling_rate = 44100

to_sample :: RealTime -> Int
to_sample t = round (RealTime.to_seconds t * sampling_rate)

-- | Emit MIDI messages that tell play_cache to get ready to start playing at
-- the given time.
--
-- TODO currently this is just the frame, which is bad because is dependent on
-- the sampling rate, and can't represent negative times.
prepare :: RealTime -> [Midi.ChannelMessage]
prepare t = [at 0, at 1, at 2, at 3]
    where
    at i = Midi.Aftertouch (fromIntegral i)
        (fromIntegral $ Bits.shiftR pos (i * 7) .&. 0x7f)
    pos = to_sample t

-- | Just to test 'prepare'.  play_cache does this internally.
midi_to_offset :: [Midi.ChannelMessage] -> Int
midi_to_offset = foldr go 0
    where
    go (Midi.Aftertouch key val) frames =
        set_offset (Midi.from_key key) (fromIntegral val) frames
    go _ frames = frames

set_offset :: Int -> Int -> Int -> Int
set_offset key val frames = cleared .|. Bits.shiftL val index
    where
    cleared = frames .&. Bits.complement (Bits.shiftL 0x7f index)
    index = key * 7
    -- unsigned int index = int(data[1]) * 7;
    -- unsigned int val = data[2];
    -- this->offsetFrames &= ~(0x7f << index);
    -- this->offsetFrames |= val << index;

start :: Midi.ChannelMessage
start = Midi.NoteOn 1 1

stop :: Midi.ChannelMessage
stop = Midi.NoteOff 1 1
