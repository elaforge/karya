-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Fire up the play-cache vst.
module Perform.Im.Play (
    play_cache_synth, qualified
    , encode_time, encode_play_config, decode_time, start, stop
) where
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import qualified Synth.Shared.Config as Shared.Config
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

to_sample :: RealTime -> Int
to_sample t =
    round $ RealTime.to_seconds t * fromIntegral Shared.Config.samplingRate

-- | Emit MIDI messages that tell play_cache to get ready to start playing at
-- the given time.
--
-- This is encoded as 'Midi.Aftertouch' where the key is the index*7 and the
-- value is the 7 bits at that index.  At a 44100 sampling rate, this can
-- address 2^(7*4) / 44100 / 60 = 101 minutes, which is long enough for
-- anything I'm likely to write.  TODO since this can't represent negative
-- times, and the DAW likely doesn't like them either, I'll have to normalize
-- the output.
encode_time :: RealTime -> [Midi.ChannelMessage]
encode_time t = [at 0, at 1, at 2, at 3]
    where
    at i = Midi.Aftertouch (fromIntegral i)
        (fromIntegral $ Bits.shiftR pos (i * 7) .&. 0x7f)
    pos = to_sample t

-- | Send the block to play, along with muted instruments, if any.  Each
-- is separated by a \0.
encode_play_config :: FilePath -> BlockId -> Set Score.Instrument
    -> [Midi.ChannelMessage]
encode_play_config score_path block_id muted =
    encode_text $ Text.intercalate "\0" $
        txt (Shared.Config.playFilename score_path block_id)
            : map ScoreT.instrument_name (Set.toList muted)

-- | Encode text in MIDI.  This uses a PitchBend to encode a pair of
-- characters, with a leading '\DEL' to mark the start of the sequence, and
-- possibly padding with a ' ' at the end.
--
-- TODO this only works for ASCII, because the PitchBend encoding is 7-bit.
encode_text :: Text -> [Midi.ChannelMessage]
encode_text text = [Midi.PitchBendInt (ord c1) (ord c2) | (c1, c2) <- name]
    where
    ord = fromIntegral . Char.ord
    -- Prepend 0x7f to tell PlayCache to clear any accumulated junk.  It
    -- happens to be \DEL, which is a nice coincidence.
    name = pairs ('\DEL' : Text.unpack text)
    pairs (x:y:xs) = (x, y) : pairs xs
    pairs [x] = [(x, ' ')]
    pairs [] = []

-- | Just to test 'encode_time'.  play_cache does this internally.
decode_time :: [Midi.ChannelMessage] -> Int
decode_time = foldr go 0
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
stop = Midi.AllNotesOff
