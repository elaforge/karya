-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Fire up the play-cache vst.
module Perform.Im.Play (
    playCacheSynth
    , encodeTime, encodePlayConfig, decodeTime, start, stop
) where
import qualified Data.Bits as Bits
import           Data.Bits ((.&.), (.|.))
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


playCacheSynth :: Inst.SynthDecl Cmd.InstrumentCode
playCacheSynth = Inst.SynthDecl
    { synthdName = InstT.synth UiConfig.playCache
    , synthdDoc = "play_cache VST, to play the output of offline synthesizers."
    , synthdPatches = [(Patch.defaultName, inst)]
    , synthdWarns = []
    }
    where
    inst = Inst.Inst
        { instBackend = Inst.Midi $ Patch.patch (0, 0) Patch.defaultName
        , instCommon = Common.doc #= doc $ Common.common Cmd.emptyCode
        }
    doc = "This dummy patch is just to give a way to configure the MIDI\
        \ channel of the play_cache VST. It's never actually played, and the\
        \ instrument should never be used in the score."

toSample :: RealTime -> Int
toSample t =
    round $ RealTime.toSeconds t * fromIntegral Shared.Config.samplingRate

-- | Emit MIDI messages that tell play_cache to get ready to start playing at
-- the given time.
--
-- This is encoded as 'Midi.Aftertouch' where the key is the index*7 and the
-- value is the 7 bits at that index.  At a 44100 sampling rate, this can
-- address 2^(7*4) / 44100 / 60 = 101 minutes, which is long enough for
-- anything I'm likely to write.  TODO since this can't represent negative
-- times, and the DAW likely doesn't like them either, I'll have to normalize
-- the output.
encodeTime :: RealTime -> [Midi.ChannelMessage]
encodeTime t = [at 0, at 1, at 2, at 3]
    where
    at i = Midi.Aftertouch (fromIntegral i)
        (fromIntegral $ Bits.shiftR pos (i * 7) .&. 0x7f)
    pos = toSample t

-- | Send the block to play, along with muted instruments, if any.  Each
-- is separated by a \0.
encodePlayConfig :: FilePath -> BlockId -> Set ScoreT.Instrument
    -> [Midi.ChannelMessage]
encodePlayConfig scorePath blockId muted =
    encodeText $ Text.intercalate "\0" $
        txt (Shared.Config.playFilename scorePath blockId)
            : map ScoreT.instrumentName (Set.toList muted)

-- | Encode text in MIDI.  This uses a PitchBend to encode a pair of
-- characters, with a leading '\DEL' to mark the start of the sequence, and
-- possibly padding with a ' ' at the end.
--
-- TODO this only works for ASCII, because the PitchBend encoding is 7-bit.
encodeText :: Text -> [Midi.ChannelMessage]
encodeText text = [Midi.PitchBendInt (ord c1) (ord c2) | (c1, c2) <- name]
    where
    ord = fromIntegral . Char.ord
    -- Prepend 0x7f to tell PlayCache to clear any accumulated junk.  It
    -- happens to be \DEL, which is a nice coincidence.
    name = pairs ('\DEL' : Text.unpack text)
    pairs (x:y:xs) = (x, y) : pairs xs
    pairs [x] = [(x, ' ')]
    pairs [] = []

-- | Just to test 'encode_time'.  play_cache does this internally.
decodeTime :: [Midi.ChannelMessage] -> Int
decodeTime = foldr go 0
    where
    go (Midi.Aftertouch key val) frames =
        setOffset (Midi.fromKey key) (fromIntegral val) frames
    go _ frames = frames

setOffset :: Int -> Int -> Int -> Int
setOffset key val frames = cleared .|. Bits.shiftL val index
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
