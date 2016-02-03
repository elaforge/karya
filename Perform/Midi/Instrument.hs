-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.

    This is all a little too complicated.

    The complete description for a set of instruments is a 'MidiDb.SynthDesc'.
    This is just a ('Synth', PatchMap) pair, and a PatchMap is a map from
    instrument name to 'Patch'.  A Patch contains an 'Instrument', which is the
    subset of data needed for performance.  Since there is a separate
    Instrument per keyswitch, multiple Instruments may be generated from
    a single Patch.  Patches also inherit some information from their Synth.
    So the hierarchy, from general to specific, goes
    @'Synth' -> 'Patch' -> 'Instrument'@.

    Creation of a SynthDesc is a little complicated because of the
    inter-relationships between the types.  A Patch is created with an
    Instrument as a template, but the template Instrument also wants know
    the synth name for error reporting, so those should be kept in sync.
-}
module Perform.Midi.Instrument (
    module Perform.Midi.Instrument, Control.PbRange
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector

import qualified Util.Lens as Lens
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Vector

import qualified Midi.Midi as Midi
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import Global
import Types



-- * config

-- | Per-score instrument configuration.
type Configs = Map.Map Score.Instrument Config

configs :: [(Score.Instrument, [Addr])] -> Configs
configs = Map.fromList . map (second config)

voice_configs :: [(Score.Instrument, [(Addr, Maybe Voices)])] -> Configs
voice_configs = Map.fromList . map (second voice_config)

get_addrs :: Score.Instrument -> Configs -> [Addr]
get_addrs inst = maybe [] (map fst . config_addrs) . Map.lookup inst

-- | Configuration for one instrument on a score.
data Config = Config {
    -- | An instrument may have multiple addresses assigned to it, which means
    -- that it can be multiplexed across multiple channels.  In addition,
    -- multiple instruments can be allocated to overlapping addresses, which is
    -- how keyswitches work; each one is considered a separate instrument.  An
    -- instrument wishing to use an address will emit an appropriate message to
    -- configure it (probably a keyswitch, possibly a program change).
    --
    -- Each Addr has a count of how many simultaneous voices the addr can
    -- handle.  Nothing means there's no limit.
    config_addrs :: ![(Addr, Maybe Voices)]
    -- | This is a local version of 'patch_restricted_environ'.
    , config_restricted_environ :: !RestrictedEnviron.Environ
    -- | This is the control equivalent to 'config_restricted_environ'.  These
    -- controls are merged when the instrument comes into scope.  They can be
    -- useful for setting default transposition, e.g. if an instrument sounds
    -- in the wrong octave.  Unlike environ there is no patch version
    -- because...  didn't seem useful?  If I need it, I can add it.
    , config_controls :: !Score.ControlValMap
    -- | A local version of 'patch_scale'.
    , config_scale :: !(Maybe Scale)
    -- | Default controls for this instrument, will always be set unless
    -- explicitly replaced.  This hopefully avoids the problem where
    -- a synthesizer starts in an undefined state.  This is different from
    -- 'config_controls' in that these are meant to provide a default for
    -- synthesizer state, so these are only applied during conversion, and
    -- thus should only contain controls the MIDI instrument understands.
    , config_control_defaults :: !Score.ControlValMap
    -- | If true, this instrument is filtered out prior to playing.
    , config_mute :: !Bool
    -- | If any instrument is soloed, all instruments except soloed ones are
    -- filtered out prior to playing.
    , config_solo :: !Bool
    } deriving (Eq, Read, Show)

config_environ :: Config -> BaseTypes.Environ
config_environ = RestrictedEnviron.convert . config_restricted_environ

addrs = Lens.lens config_addrs
    (\f r -> r { config_addrs = f (config_addrs r) })
cenviron = Lens.lens config_restricted_environ
    (\f r -> r { config_restricted_environ = f (config_restricted_environ r) })
controls = Lens.lens config_controls
    (\f r -> r { config_controls = f (config_controls r) })
cscale = Lens.lens config_scale
    (\f r -> r { config_scale = f (config_scale r) })
control_defaults = Lens.lens config_control_defaults
    (\f r -> r { config_control_defaults = f (config_control_defaults r) })
mute = Lens.lens config_mute
    (\f r -> r { config_mute = f (config_mute r) })
solo = Lens.lens config_solo
    (\f r -> r { config_solo = f (config_solo r) })

config1 :: Midi.WriteDevice -> Midi.Channel -> Config
config1 dev chan = config [(dev, chan)]

config :: [Addr] -> Config
config = voice_config . map (, Nothing)

voice_config :: [(Addr, Maybe Voices)] -> Config
voice_config addrs = Config
    { config_addrs = addrs
    , config_restricted_environ = mempty
    , config_controls = mempty
    , config_scale = Nothing
    , config_control_defaults = mempty
    , config_mute = False
    , config_solo = False
    }

instance Pretty.Pretty Config where
    format (Config addrs environ controls scale control_defaults mute solo) =
            Pretty.record "Config"
        [ ("addrs", Pretty.format addrs)
        , ("environ", Pretty.format environ)
        , ("controls", Pretty.format controls)
        , ("scale", Pretty.format scale)
        , ("mute", Pretty.format mute)
        , ("control_defaults", Pretty.format control_defaults)
        , ("solo", Pretty.format solo)
        ]

-- | MIDI instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)
-- | Number of simultaneous voices a certain Addr supports, aka polyphony.
type Voices = Int

-- * instrument db types

-- When there are multiple backends, this will have to move to a more general
-- place.

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its 'Synth'.
data Patch = Patch {
    -- | This is the name of the instrument on the synthesizer, and likely has
    -- all sorts of wacky characters in it, and may not be unique, even on
    -- a single synth.  This is just for reference, and is not actually used by
    -- anyone.  But since 'inst_score' might have been heavily mangled to fit
    -- into Score.Instrument naming restrictions, it's still useful to keep the
    -- original name around.
    patch_name :: !Text
    , patch_control_map :: !Control.ControlMap
    , patch_pitch_bend_range :: !Control.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , patch_decay :: !(Maybe RealTime)
    , patch_scale :: !(Maybe Scale)
    , patch_flags :: !(Set.Set Flag)
    , patch_initialize :: !InitializePatch
    , patch_attribute_map :: !AttributeMap
    , patch_call_map :: !CallMap
    } deriving (Eq, Show)

instance Pretty.Pretty Patch where
    format (Patch name cmap pb_range decay scale flags init attr_map call_map) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("control_map", Pretty.format cmap)
            , ("pitch_bend_range", Pretty.format pb_range)
            , ("decay", Pretty.format decay)
            , ("scale", Pretty.format scale)
            , ("flags", Pretty.format flags)
            , ("initialize", Pretty.format init)
            , ("attribute_map", Pretty.format attr_map)
            , ("call_map", Pretty.format call_map)
            ]

name = Lens.lens patch_name (\f r -> r { patch_name = f (patch_name r) })
control_map = Lens.lens patch_control_map
    (\f r -> r { patch_control_map = f (patch_control_map r) })
pitch_bend_range = Lens.lens patch_pitch_bend_range
    (\f r -> r { patch_pitch_bend_range = f (patch_pitch_bend_range r) })
decay = Lens.lens patch_decay (\f r -> r { patch_decay = f (patch_decay r) })
scale = Lens.lens patch_scale (\f r -> r { patch_scale = f (patch_scale r) })
flags = Lens.lens patch_flags
    (\f r -> r { patch_flags = f (patch_flags r) })
initialize = Lens.lens patch_initialize
    (\f r -> r { patch_initialize = f (patch_initialize r) })
attribute_map = Lens.lens patch_attribute_map
    (\f r -> r { patch_attribute_map = f (patch_attribute_map r) })
call_map = Lens.lens patch_call_map
    (\f r -> r { patch_call_map = f (patch_call_map r) })

-- | Create a Patch with empty vals, to set them as needed.
patch :: Control.PbRange -> InstTypes.Name -> Patch
patch pb_range name = Patch
    { patch_name = name
    , patch_control_map = mempty
    , patch_pitch_bend_range = pb_range
    , patch_decay = Nothing
    , patch_scale = Nothing
    , patch_flags = Set.empty
    , patch_initialize = NoInitialization
    , patch_attribute_map = Common.AttributeMap []
    , patch_call_map = Map.empty
    }

-- | This is a convention for the default instrument of a synth.  This is
-- useful for softsynths whose patches all generally have the same config.
default_name :: InstTypes.Name
default_name = ""

-- | Map attributes to the names of the calls they should map to.  This
-- is used by the integrator to turn score events into UI events.
type CallMap = Map.Map Score.Attributes BaseTypes.CallId

-- | If a patch is tuned to something other than 12TET, this vector maps MIDI
-- key numbers to their NNs, or 0 if the patch doesn't support that key.
data Scale = Scale !Text (Vector.Vector Double)
    deriving (Eq, Show, Read)

instance Pretty.Pretty Scale where
    pretty (Scale name v) = name <> " ("
        <> showt (Util.Vector.count (/=0) v) <> " pitches)"

scale_keys :: Scale -> [(Midi.Key, Pitch.NoteNumber)]
scale_keys (Scale _ nns) =
    map (second Pitch.nn) $ filter ((/=0) . snd) $ zip [0..] $ Vector.toList nns

-- | Fill in non-adjacent MIDI keys by interpolating the neighboring
-- NoteNumbers.  This is because a 0 between two notes will prevent pitch
-- slides.  Another problem is that the MIDI performer has no notion of
-- instruments that don't support certain key numbers.  That could be added
-- but it's simpler to just not have patches like that.
make_scale :: Text -> [(Midi.Key, Pitch.NoteNumber)] -> Scale
make_scale name keys =
    Scale name (empty Vector.// map convert (interpolate keys))
    where
    convert (k, Pitch.NoteNumber nn) = (Midi.from_key k, nn)
    interpolate ((k1, nn1) : rest@((k2, nn2) : _))
        | k1 + 1 == k2 = (k1, nn1) : interpolate rest
        | otherwise = (k1, nn1) : map mk (Seq.range' (k1+1) k2 1)
            ++ interpolate rest
        where
        mk k = (k, nn)
            where
            nn = Num.scale nn1 nn2 $ Num.normalize
                (Midi.from_key k1) (Midi.from_key k2) (Midi.from_key k)
    interpolate xs = xs
    empty = Vector.fromList $ replicate 128 0

convert_scale :: Scale -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
convert_scale (Scale _ scale) (Pitch.NoteNumber nn) =
    case Util.Vector.bracketing scale nn of
        Just (i, low, high) | low /= 0 -> Just $ Pitch.NoteNumber $
            fromIntegral i + Num.normalize low high nn
        _ -> Nothing

set_flag :: Flag -> Patch -> Patch
set_flag flag = flags %= Set.insert flag

unset_flag :: Flag -> Patch -> Patch
unset_flag flag = flags %= Set.delete flag

triggered, pressure :: Patch -> Patch
triggered = set_flag Triggered
pressure = set_flag Pressure

has_flag :: Patch -> Flag -> Bool
has_flag inst flag = Set.member flag (patch_flags inst)

-- | Various instrument flags.
data Flag =
    -- | Patch doesn't pay attention to duration.  E.g., drum samples may not
    -- pay attention to note off.  The UI can use this to create zero duration
    -- events for this patch.
    Triggered
    -- | Patch uses continuous pressure control, assigned to CC 2 (breath),
    -- instead of trigger velocity.  This is used to support the @dyn@ control.
    -- Percussive instruments like pianos map it to MIDI velocity, and
    -- continuous instruments like winds always have maximum velocity and map
    -- @dyn@ to breath.
    | Pressure
    -- | Notes on this instrument don't change their pitch after they start.
    -- This suppresses all pitch bending for each note.  It's useful because
    -- it can be difficult to prevent pitch leakage.  E.g. if a transpose
    -- signal starts after the note and the note is moved, it winds up at the
    -- end of the note.
    --
    -- Obviously it's a hack, but it's useful in practice.  It could also go in
    -- the note generator, but it's convenient to apply it in convert because
    -- that's where the transpose signals are applied.
    | ConstantPitch
    -- | If true, a keysitch has to be held while its note is playing.
    -- Otherwise, it will just be tapped before the note starts.
    | HoldKeyswitch
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Flag where pretty = showt

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch =
    -- | Send these msgs to initialize the patch.  Should be a patch change or
    -- a sysex.
    InitializeMidi ![Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage !Text
    | NoInitialization
    deriving (Eq, Ord, Show)

instance Pretty.Pretty InitializePatch where
    format (InitializeMidi msgs) =
        Pretty.text "InitializeMidi" Pretty.<+> Pretty.format msgs
    format init = Pretty.text (showt init)

-- ** AttributeMap

-- | This is a specialization of 'Common.AttributeMap' for MIDI.
-- Should use a constructor like 'keyswitches', which will call
-- 'Common.sort_attributes' to make sure there are no overlaps.
type AttributeMap = Common.AttributeMap ([Keyswitch], Maybe Keymap)

-- | A Keymap corresponds to a timbre selected by MIDI key range, rather than
-- keyswitches.  Unlike a keyswitch, this doesn't change the state of the MIDI
-- channel, so multiple keymapped notes can coexist, and keymap replaces the
-- pitch of the note.
data Keymap =
    -- | This ignores the event's pitch and instead emits the given MIDI key.
    -- This is appropriate for drumkit style patches, with a separate unpitched
    -- timbre on each key.
    UnpitchedKeymap !Midi.Key
    -- | The timbre is mapped over the inclusive MIDI key range from low to
    -- high, where the pitch of the low end of the range is given by the
    -- NoteNumber.  So this transposes the event's pitch and clips it to the
    -- given range.
    | PitchedKeymap !Midi.Key !Midi.Key !Pitch.NoteNumber
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Keymap where
    pretty (UnpitchedKeymap k) = pretty k
    pretty (PitchedKeymap low high nn) = pretty low <> "--"
        <> pretty high <> "(" <> pretty nn <> ")"

-- | A Keyswitch changes the timbre of a patch, but does so in a channel-global
-- way.  So overlapping notes with different keyswitches will be split into
-- different channels, if possible.
data Keyswitch =
    Keyswitch !Midi.Key
    -- | This keyswitch is triggered by a control change.
    | ControlSwitch !Midi.Control !Midi.ControlValue
    -- | This is like 'ControlSwitch', except send a poly aftertouch value
    -- for the note's pitch.  This allows simultaneous different notes with
    -- different articulations.
    | Aftertouch !Midi.ControlValue
    deriving (Eq, Ord, Show, Read)

instance DeepSeq.NFData Keymap where
    rnf (UnpitchedKeymap k) = k `seq` ()
    rnf (PitchedKeymap k _ _) = k `seq` ()

instance DeepSeq.NFData Keyswitch where
    rnf k = k `seq` () -- already strict

instance Pretty.Pretty Keyswitch where
    format (Keyswitch key) = "key:" <> Pretty.format key
    format (ControlSwitch cc val) =
        "cc:" <> Pretty.format cc <> "/" <> Pretty.format val
    format (Aftertouch val) = "at:" <> Pretty.format val

-- | An AttributeMap with just keyswitches.
keyswitches :: [(Score.Attributes, [Keyswitch])] -> AttributeMap
keyswitches attr_ks =
    Common.attribute_map [(attrs, (ks, Nothing)) | (attrs, ks) <- attr_ks]

-- | An AttributeMap with a single Midi.Key keyswitch per Attribute.
single_keyswitches :: [(Score.Attributes, Midi.Key)] -> AttributeMap
single_keyswitches = keyswitches . map (second ((:[]) . Keyswitch))

cc_keyswitches :: Midi.Control -> [(Score.Attributes, Midi.ControlValue)]
    -> AttributeMap
cc_keyswitches cc = keyswitches . map (second ((:[]) . ControlSwitch cc))

keymap :: [(Score.Attributes, Keymap)] -> AttributeMap
keymap table =
    Common.attribute_map [(attr, ([], Just keymap)) | (attr, keymap) <- table]

-- | An AttributeMap with just unpitched keymaps.
unpitched_keymap :: [(Score.Attributes, Midi.Key)] -> AttributeMap
unpitched_keymap = keymap . map (second UnpitchedKeymap)
