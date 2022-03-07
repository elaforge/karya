-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Sample where
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Thru as Thru
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


-- | Path to a sample, relative to the instrument db root.
type SamplePath = FilePath

-- | Low level representation of a note.  This corresponds to a single sample
-- played.
data Note = Note {
    start :: !Audio.Frames
    -- | This is the actual duration of the sample at the given 'ratios', not
    -- the requested 'Note.duration'.
    -- TODO maybe move 'duration' to Sample then.
    , duration :: !Audio.Frames
    , effectControls :: Map Control.Control Signal.Signal
    , sample :: Sample
    -- | Hash of (start, duration, effectControls, sample).  Putting it here
    -- means I can memoize its creation but also that changing Note will make
    -- it out of sync.
    , hash :: Note.Hash
    } deriving (Show)

end :: Note -> Audio.Frames
end note = start note + duration note

note :: Audio.Frames -> Audio.Frames -> Map Control.Control Signal.Signal
    -> Sample -> Note
note start dur effectControls sample = Note
    { start = start
    , duration = dur
    , effectControls = effectControls
    , sample = sample
    , hash = makeHash start dur effectControls sample
    }

makeHash :: Audio.Frames -> Audio.Frames -> Map Control.Control Signal.Signal
    -> Sample -> Note.Hash
makeHash start dur effectControls sample = mconcat $
    map Note.hashBytes [e start, e dur, e effectControls, e sample]
    -- TODO ensure envelope and ratios are clipped to (start, duration)?
    where
    e :: Serialize.Serialize a => a -> ByteString.ByteString
    e = Serialize.encode

-- | The actual sample played by a 'Note'.
data Sample = Sample {
    -- | This is initially relative to 'Patch._rootDir', and will have the root
    -- dir prepended before rendering.
    filename :: !SamplePath
    -- | Sample start offset.
    , offset :: !Audio.Frames
    -- | The sample ends when it runs out of samples, or when envelope ends
    -- on 0.
    , envelope :: !Signal.Signal
    , pan :: !Signal.Signal
    -- | Sample rate conversion ratio.  This controls the pitch.
    , ratios :: !Signal.Signal
    , stretch :: !Stretch
    } deriving (Show)

data Stretch = Stretch {
    stretchMode :: !StretchMode
    , timeRatio :: !Signal.Y
    , pitchRatio :: !Signal.Y
    } deriving (Show)

-- | This maps to [Rubberband.Option].  It's indirect to avoid a dependency on
-- RubberbandC, and hence the C library.
data StretchMode = StretchDefault | StretchPercussive
    deriving (Show, Enum, Bounded)

make :: SamplePath -> Sample
make filename = Sample
    { filename = filename
    , offset = 0
    , envelope = Signal.constant 1
    , pan = Signal.constant 0
    , ratios = Signal.constant 1
    , stretch = Stretch
        { stretchMode = StretchDefault
        , timeRatio = 1
        , pitchRatio = 1
        }
    }

modifyFilename :: (SamplePath -> SamplePath) -> Sample -> Sample
modifyFilename modify sample = sample { filename = modify (filename sample) }

instance Pretty Note where
    format (Note start dur effectControls sample hash) = Pretty.record "Note"
        [ ("start", Pretty.format start)
        , ("duration", Pretty.format dur)
        , ("effectControls", Pretty.format effectControls)
        , ("sample", Pretty.format sample)
        , ("hash", Pretty.format hash)
        ]

instance Pretty Sample where
    format (Sample filename offset envelope pan ratios stretch) =
        Pretty.record "Sample"
            [ ("filename", Pretty.format filename)
            , ("offset", Pretty.format offset)
            , ("envelope", Pretty.format envelope)
            , ("pan", Pretty.format pan)
            , ("ratios", Pretty.format ratios)
            , ("stretch", Pretty.format stretch)
            ]

instance Pretty Stretch where
    format (Stretch mode time pitch) = Pretty.record "Stretch"
        [ ("stretchMode", Pretty.text (showt mode))
        , ("timeRatio", Pretty.format time)
        , ("pitchRatio", Pretty.format pitch)
        ]

-- | Like Pretty Note, but shorter.
prettyNote :: Note -> Text
prettyNote note = pretty
    ( start note, duration note, hash note
    , FilePath.takeFileName (filename (sample note))
    )

instance Serialize.Serialize Sample where
    put (Sample a b c d e f) =
        Serialize.put a >> Serialize.put b >> Serialize.put c >> Serialize.put d
        >> Serialize.put e >> Serialize.put f
    get = fail "no get for Sample"

instance Serialize.Serialize Stretch where
    put (Stretch a b c) = Serialize.put a >> Serialize.put b >> Serialize.put c
    get = fail "no get for Stretch"

instance Serialize.Serialize StretchMode where
    put = Serialize.put_enum_old
    get = Serialize.get_enum_old

-- | The duration of a note which plays the entire sample.  This should be
-- longer than any sample, and will be clipped to sample duration.
forever :: RealTime
forever = 1000

-- * util

-- | This is the resampling ratio, which is inverse to the pitch ratio, which
-- is pretty confusing.  E.g.  When I go up *2, I should be skipping every
-- other sample.  So srate should be *2.  Number of frames is /2.  So the
-- resampling ratio for +12nn is 1/2, while the pitch ratio is 2.
pitchToRatio :: Pitch.NoteNumber -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleNn nn = Pitch.nn_to_hz sampleNn / Pitch.nn_to_hz nn

pitchToRatioSignal :: Pitch.NoteNumber -> Note.Note -> Signal.Signal
pitchToRatioSignal sampleNn =
    Signal.map_y srate (pitchToRatio sampleNn . Pitch.nn) . fromMaybe mempty
    . Map.lookup Control.pitch . Note.controls

relativePitchToRatio :: Pitch.NoteNumber -> Signal.Y
relativePitchToRatio offset = pitchToRatio 60 (60 + offset)
    -- Surely there's a way to do this without the fake pitch?
    -- Yes, invert ratioToPitch, but I should consistently use pitch ratio,
    -- and rename pitchToRatio to pitchToResampleRatio

-- | This is pitch ratio, not resample ratio!
ratioToPitch :: Double -> Pitch.NoteNumber
ratioToPitch ratio = Pitch.nn $ logBase 2 ratio * 12

srate :: RealTime
srate = 1/8

toThru :: FilePath -> Sample -> Thru.Play
toThru sampleDir sample = Thru.Play
    { _sample = Config.unsafeSamplerRoot </> sampleDir </> filename sample
    , _offset = fromIntegral $ offset sample
    , _ratio = Signal.at start $ ratios sample
    , _volume = Signal.at start $ envelope sample
    }
    where start = 0
