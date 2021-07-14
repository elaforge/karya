-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This has the protocol to talk to a running play_cache and tell it to play
-- samples in realtime.  This is for the audio preview aka "MIDI thru" feature
-- for im.  Since each im patch may respond in its own way to a Note, this
-- relies on the patch itself exporting a 'ThruFunction' to find the
-- appropriate sample.
--
-- This used to use OSC, but it turned out OSC wasn't really getting me
-- anything, and its restrictions were troublesome, so now it's a custom
-- format as emitted by 'serialize'.
module Synth.Shared.Thru (
    ThruFunction, Note(..)
    , Message(..), Play(..)
    , send
) where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Util.Network as Network
import qualified Derive.Attrs as Attrs
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Config as Config

import           Global


-- | This is a specialized version of 'Cmd.Cmd.ThruFunction'.  Being more
-- specialized means I don't have to directly depend on "Cmd.Cmd" from here.
type ThruFunction = [Note] -> Either Error Message

data Message = Plays [Play] | Stop
    deriving (Show)

data Note = Note {
    _pitch :: !Pitch.NoteNumber
    , _velocity :: !Double
    , _attributes :: !Attrs.Attributes
    , _startOffset :: !Int
    } deriving (Show)

type Error = Text
type Frames = Int

data Play = Play {
    _sample :: !FilePath
    , _offset :: !Frames
    , _ratio :: !Double
    , _volume :: !Double
    } deriving (Eq, Show)

send :: Message -> IO ()
send msg = Network.withConnection_ (Network.IP Config.thruPort) $ \hdl ->
    ByteString.hPut hdl $ serialize msg

-- | This serializes to a protocol with null-terminated fields, where a message
-- is terminated with '\n'.  That makes it easy to read with getline(), and
-- easy to parse each field as a string.
serialize :: Message -> Char8.ByteString
serialize (Plays plays) =
    mconcatMap (<>"\0") (concatMap serialize1 plays) <> "\n"
    where
    serialize1 (Play sample offset ratio volume) =
        map Char8.pack [sample, show offset, show ratio, show volume]
serialize Stop = "stop\n"


{- NOTE [realtime-im]

    Original design for realtime im / im preview.  I wound up doing "hybrid im
    and MIDI", and using OSC for the protocol to talk to play_cache.
    [ And now I'm no longer using OSC, but a a simple custom message format. ]

    . Get the Sampler.Patch, run its convert function, then send it to
      play_cache.  play_cache only needs use filename, ratio, vol.
      . I can use OSC: /start/path/to/sample <ratio> <vol>
        Abort a sample on space: /stop
      . The advantage is that I can use liblo for C++, and some haskell
        library for karya.
      Implement with ignoring ratio, just stream the sample.
        . I should be able to reuse Streamer.
        . Start a separate thread, which listens on an OSC socket, creates
          a Streamer, and has it start streaming.
        . The main process() also mixes in any output from the realtime
          Streamer.
        . Since it's realtime, I don't want to keep track of debt in read(),
          I just want to get samples as soon as they arrive.
        . Also I play a file, not a directory, and I need to apply volume and
          ratio.
        * plug into PlayCache
      * I need a "MIDI" thru which takes Input.Note, runs the patch convert,
        and sends OSC.
      * Add support for ratio.  libsamplerate with a cheap quality should be
        able to do realtime.
        . I need to do the resampling in the streamer thread.
        . Rename Sample to Audio or Stream
        . I think I can change Streamer to take an abstract Audio in start(),
          then make Mix into an Audio.  Then I can get rid of synchronized.
        . Mix becomes Tracks, since it mixes tracks, and deals with the
          directory structure.
        . Now make a Resample : Audio, and
          . Osc: Resample(ratio, SampleFile(fname))
          . PlayCache: Tracks(dir, mutes), Track hardcodes SampleDirectory
        . But I still need to call Streamer::start with no allocation.  How it
          does that currently is dir and mutes are statically allocated and
          just assigned on start().  The the actual initialization is delayed
          until streamLoop(), which is non-realtime.  The pattern is pass
          constructor and arguments, and call it in streamLoop.
        . I could emulate that with a separate initialiaze method.  I can't
          think of how to get it to work though.
        . Alternately, use Streamer subclasses.
    overview:
      . Im instruments should have a midi thru-like preview sound.
      . This doesn't have to be performance-level of low latency,
        probably < 100ms will do.  Still, the lower the better.
      ? Experiment with how various latencies feel, so I know my target.
      . If I do hybrid im and MIDI: below then I need to make play_cache into a
        little sampler, which just means streaming through libsamplerate.
    possible designs:
      parallel MIDI:
        . Assign a parallel MIDI instrument for thru.
        . All I get is the right pitch and possibly similar instrument sound.
        . Probably in practice I'd just have a single MIDI instrument with
          a neutral sound.
      hybrid im and MIDI:
        . A compromise would be a general mechanism to render samples, and
          then feed that into a realtime sampler.  Of course commercial
          samplers can do that but would insert a lot of manual work into
          the process.  So maybe I can write a minimal MIDI VST sampler.
        . All it needs to do is play a sample with a given pitch (pitch bend
          and pitch) and velocity.
        . For this, I need to prepare each instrument by rendering a set of
          example samples.
        . I don't want to have to encode notes as MIDI, so just have
          play_cache open a socket, which takes a filename and pitch and
          immediately plays whatever it gets.  All I have to do is teach
          play_cache to resample and maybe scale volume.
        . Maybe I just run the convert function, and then take only the
          Sample.filename, and send it to play_cache.
      pure im:
        . I have to give a single Note to the im synthesizer, which then has
          to stream to the VST, which has a socket to immediately play
          samples.
        . I might have to run im synthesizers persistently to avoid startup
          overhead.
        . Presumably I also have to ensure they use a sufficiently small
          block size to get that first block out quickly.  Since I'm not
          doing realtime control, I don't actually care about the rest of
          the blocks.
        . This is more complicated, but is accurate for the instrument:
          I get the right sounds so I can e.g. see how sampler-im picks
          samples in realtime-ish.  However, if I go this far, it might not
          be that much more work to go for the full realtime interface.
      realtime interface:
        . This is basically try to turn im into a realtime interface.
        . The difference with the previous is that I'd also support realtime
          control signals.
        . The reason to want this is that I could explore instruments
          interactively.
        . I would probably have to take MIDI or OSC, run the im synth
          persistently, and either stream to the VST or directly to audio
          out.
        . I'd have to plan out a general purpose interface.  Synthesizers
          that use knowledge of the future would have to be adapted to not
          having that.
-}
