-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.
-}
module Perform.Midi.Patch (
    -- * Config
    Config(..), config_addrs
    , patch_to_config, merge_defaults
    , allocation, control_defaults, initialization, settings
    , config
    , Initialization(..), Addr, Voices
    , has_flag
    -- Re-exported so instrument definitions don't have to have
    -- Midi.Control.PbRange.
    , Control.PbRange
    -- ** Settings
    , Settings(..), make_settings
    , pitch_bend_range, decay, scale, flags

    -- * Patch
    , Patch(..), name, control_map
    , initialize, attribute_map, call_map, defaults
    , patch
    , default_name
    , CallMap
    -- ** Scale
    , Scale(..) -- should just be Scale(scale_name), but Cmd.Serialize
    , make_scale
    , convert_scale, nn_at
    , scale_nns, scale_offsets, scale_tuning
    -- ** Flag
    , Flag(..)
    , add_flag, remove_flag
    -- ** InitializePatch
    , InitializePatch(..)
    -- ** AttributeMap
    , AttributeMap, Keymap(..), Keyswitch(..)
    , keyswitches, single_keyswitches, cc_keyswitches, keymap, unpitched_keymap
    , keyswitch_on, keyswitch_off
#ifdef TESTING
    , module Perform.Midi.Patch
#endif
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import qualified Util.Lens as Lens
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.Vector

import qualified Midi.Midi as Midi
import qualified Derive.Attrs as Attrs
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import Global
import Types


-- * Config

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
    config_allocation :: ![(Addr, Maybe Voices)]
    -- | Default controls for this instrument, will always be set unless
    -- explicitly replaced.  This hopefully avoids the problem where
    -- a synthesizer starts in an undefined state.  This is different from
    -- 'Common.config_controls' in that these are meant to provide a default
    -- for synthesizer state, so these are only applied during conversion, and
    -- thus should only contain controls the MIDI instrument understands.
    , config_control_defaults :: !Score.ControlValMap
    , config_initialization :: !(Set Initialization)
    , config_settings :: !Settings
    } deriving (Eq, Read, Show)

allocation = Lens.lens config_allocation
    (\f r -> r { config_allocation = f (config_allocation r) })
control_defaults = Lens.lens config_control_defaults
    (\f r -> r { config_control_defaults = f (config_control_defaults r) })
initialization = Lens.lens config_initialization
    (\f r -> r { config_initialization = f (config_initialization r) })
settings = Lens.lens config_settings
    (\f r -> r { config_settings = f (config_settings r) })

config_addrs :: Config -> [Addr]
config_addrs = map fst . config_allocation

config :: Settings -> [(Addr, Maybe Voices)] -> Config
config settings alloc = Config
    { config_allocation = alloc
    , config_control_defaults = mempty
    , config_initialization = mempty
    , config_settings = settings
    }

patch_to_config :: Patch -> [(Addr, Maybe Voices)] -> Config
patch_to_config patch = config (patch_defaults patch)

merge_defaults :: Patch -> Config -> Config
merge_defaults patch = settings %= (<> patch_defaults patch)

instance Pretty Config where
    format (Config alloc scale control_defaults initialization) =
        Pretty.record "Config"
            [ ("allocation", Pretty.format alloc)
            , ("scale", Pretty.format scale)
            , ("control_defaults", Pretty.format control_defaults)
            , ("initialization", Pretty.format initialization)
            ]

-- | Document what kinds of initialization this instrument needs.  Each
-- instrument is initialized once when the score is loaded.
data Initialization =
    Tuning -- ^ Configure tuning with 'Midi.realtime_tuning'.
    | NrpnTuning -- ^ Configure tuning with 'Midi.nrpn_tuning'.
    | Midi -- ^ Send 'InitializePatch'.
    deriving (Read, Show, Eq, Ord, Bounded, Enum)
instance Pretty Initialization where pretty = showt

-- | MIDI instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)
-- | Number of simultaneous voices a certain Addr supports, aka polyphony.
type Voices = Int

has_flag :: Config -> Flag -> Bool
has_flag config flag = Set.member flag (settings#flags #$ config)

-- ** Settings

-- | This has instrument configuration which has built-in defaults but can also
-- be modified per score.  When the instrument is allocated, 'patch_defaults'
-- is copied to 'config_settings'.
data Settings = Settings {
    config_flags :: !(Set Flag)
    , config_scale :: !(Maybe Scale)
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , config_decay :: !(Maybe RealTime)
    , config_pitch_bend_range :: !Control.PbRange
    } deriving (Eq, Read, Show)

instance Pretty Settings where
    format (Settings flags scale decay pb_range) = Pretty.record "Settings"
        [ ("flags", Pretty.format flags)
        , ("scale", Pretty.format scale)
        , ("decay", Pretty.format decay)
        , ("pitch_bend_range", Pretty.format pb_range)
        ]

instance Semigroup Settings where
    (<>)    (Settings flags1 scale1 decay1 pb_range1)
            (Settings flags2 scale2 decay2 pb_range2) =
        Settings (flags1 <> flags2) (scale1 <|> scale2) (decay1 <|> decay2)
            (if pb_range1 == no_pb_range then pb_range2 else pb_range1)
instance Monoid Settings where
    mempty = make_settings no_pb_range
    mappend = (<>)

-- | This is a special magic value to indicate an incomplete Settings, which
-- is what mempty is.  Normally 'Config' should be initialized from Settings in
-- 'patch_defaults', but 'Cmd.Instrument.MidiInst.config' is used to create
-- template allocations which are merged with the patch defaults when the
-- allocation is created.
no_pb_range :: Control.PbRange
no_pb_range = (100, -100)

make_settings :: Control.PbRange -> Settings
make_settings pb_range = Settings
    { config_flags = Set.empty
    , config_scale = Nothing
    , config_decay = Nothing
    , config_pitch_bend_range = pb_range
    }

pitch_bend_range = Lens.lens config_pitch_bend_range
    (\f r -> r { config_pitch_bend_range = f (config_pitch_bend_range r) })
decay = Lens.lens config_decay
    (\f r -> r { config_decay = f (config_decay r) })
scale = Lens.lens config_scale
    (\f r -> r { config_scale = f (config_scale r) })
flags = Lens.lens config_flags
    (\f r -> r { config_flags = f (config_flags r) })

-- * Patch

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its 'Synth'.
data Patch = Patch {
    -- | This is the name of the instrument on the synthesizer, and likely has
    -- all sorts of wacky characters in it, and may not be unique, even on
    -- a single synth.  This is just for documentation, and is not actually
    -- used by anyone, though the unique name may be derived from this one.
    --
    -- The patch's unique name, as used by 'InstTypes.Qualified' to look up
    -- the patch, is in 'Instrument.Inst.synth_insts'.
    patch_name :: !Text
    , patch_control_map :: !Control.ControlMap
    , patch_initialize :: !InitializePatch
    , patch_attribute_map :: !AttributeMap
    -- TODO this should move to Instrument.Common
    , patch_call_map :: !CallMap
    , patch_defaults :: !Settings
    } deriving (Eq, Show)

instance Pretty Patch where
    format (Patch name cmap init attr_map call_map defaults) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("control_map", Pretty.format cmap)
            , ("initialize", Pretty.format init)
            , ("attribute_map", Pretty.format attr_map)
            , ("call_map", Pretty.format call_map)
            , ("defaults", Pretty.format defaults)
            ]

name = Lens.lens patch_name (\f r -> r { patch_name = f (patch_name r) })
control_map = Lens.lens patch_control_map
    (\f r -> r { patch_control_map = f (patch_control_map r) })
initialize = Lens.lens patch_initialize
    (\f r -> r { patch_initialize = f (patch_initialize r) })
attribute_map = Lens.lens patch_attribute_map
    (\f r -> r { patch_attribute_map = f (patch_attribute_map r) })
call_map = Lens.lens patch_call_map
    (\f r -> r { patch_call_map = f (patch_call_map r) })
defaults = Lens.lens patch_defaults
    (\f r -> r { patch_defaults = f (patch_defaults r) })

-- | Create a Patch with empty vals, to set them as needed.
patch :: Control.PbRange -> InstTypes.Name -> Patch
patch pb_range name = Patch
    { patch_name = name
    , patch_control_map = mempty
    , patch_initialize = NoInitialization
    , patch_attribute_map = Common.AttributeMap []
    , patch_call_map = Map.empty
    , patch_defaults = make_settings pb_range
    }

-- | This is a convention for the default instrument of a synth.  This is
-- useful for softsynths whose patches all generally have the same config.
default_name :: InstTypes.Name
default_name = ""

-- | Map attributes to the names of the calls they should map to.  This
-- is used by the integrator to turn score events into UI events.
type CallMap = Map Attrs.Attributes Expr.Symbol

-- ** Scale

{- | Describe the tuning of a MIDI patch.

    This is used both to describe a patch tuned to something other than 12TET,
    and to retune a 12TET patch.

    The Scale is used during performance to warp played pitches to the patch's
    tuning.  The idea is that they will warp to integral 'Midi.Key's that won't
    need any tuning and can thus all go on a single MIDI channel.
-}
data Scale = Scale {
    scale_name :: !Text
    -- | If a patch is tuned to something other than 12TET, this vector maps
    -- MIDI key numbers to their NNs, or 'no_pitch' if the patch doesn't
    -- support that key.
    , scale_key_to_nn :: !(Unboxed.Vector Double)
    } deriving (Eq, Read, Show)

instance Pretty Scale where
    format (Scale name key_to_nn) = Pretty.record "Patch.Scale"
        [ ("name", Pretty.format name)
        , ("key_to_nn", Pretty.format key_to_nn)
        ]

no_pitch :: Double
no_pitch = -1

-- | Fill in non-adjacent MIDI keys by interpolating the neighboring
-- NoteNumbers.  This is because a 0 between two notes will prevent pitch
-- slides.  Another problem is that the MIDI performer has no notion of
-- instruments that don't support certain key numbers.  That could be added
-- but it's simpler to just not have patches like that.
make_scale :: Text -> [(Midi.Key, Pitch.NoteNumber)] -> Scale
make_scale name keys = Scale
    { scale_name = name
    , scale_key_to_nn = empty Unboxed.// map convert (interpolate_gaps keys)
    }
    where
    convert (k, Pitch.NoteNumber nn) = (Midi.from_key k, nn)
    empty = Unboxed.fromList $ replicate 128 no_pitch

interpolate_gaps :: [(Midi.Key, Pitch.NoteNumber)]
    -> [(Midi.Key, Pitch.NoteNumber)]
interpolate_gaps ((k1, nn1) : rest@((k2, nn2) : _))
    | k1 + 1 == k2 = (k1, nn1) : interpolate_gaps rest
    | otherwise = (k1, nn1) : map mk (Seq.range' (k1+1) k2 1)
        ++ interpolate_gaps rest
    where
    mk k = (k, nn)
        where
        nn = Num.scale nn1 nn2 $ Num.normalize
            (Midi.from_key k1) (Midi.from_key k2) (Midi.from_key k)
interpolate_gaps xs = xs

convert_scale :: Scale -> Pitch.NoteNumber -- ^ if you want this pitch
    -> Maybe Pitch.NoteNumber -- ^ play this key
convert_scale (Scale _ scale) (Pitch.NoteNumber nn) =
    case Util.Vector.bracket scale nn of
        Just (i, low, high) | low /= no_pitch ->
            Just $ Pitch.NoteNumber $ fromIntegral i + Num.normalize low high nn
        _ -> Nothing

-- *** tuning

-- | Absolute NoteNumber for each 'Midi.Key' to tune 12TET to this scale.
scale_nns :: Maybe AttributeMap -> Scale -> [(Midi.Key, Pitch.NoteNumber)]
scale_nns attr_map scale =
    [(key, nn) | (key, Just (_, nn)) <- zip [0..] (scale_tuning attr_map scale)]

-- | Relative NoteNumber offset for each 'Midi.Key' to tune 12TET to this scale.
scale_offsets :: Maybe AttributeMap -> Scale -> [Maybe Pitch.NoteNumber]
scale_offsets attr_map = map (fmap to_offset) . scale_tuning attr_map
    where to_offset (base, nn) = nn - Midi.from_key base

-- | Map the mapped keys through the scale.
scale_tuning :: Maybe AttributeMap -> Scale
    -> [Maybe (Midi.Key, Pitch.NoteNumber)]
scale_tuning attr_map scale = map tuning [0..127]
    where
    tuning key
        | null ranges = (key,) <$> nn_at scale key
        | otherwise = case List.find (in_range key) ranges of
            Nothing -> Nothing
            Just (low, _, base_nn) -> (abs_key,) <$> nn_at scale abs_key
                where abs_key = base_nn + (key - low)
    in_range key (low, high, _) = low <= key && key <= high
    ranges =
        [ (low, high, nn)
        | (_, Just (PitchedKeymap low high nn))
            <- maybe [] Common.attribute_vals attr_map
        ]

nn_at :: Scale -> Midi.Key -> Maybe Pitch.NoteNumber
nn_at scale key
    | key >= 0 && k < Unboxed.length (scale_key_to_nn scale) =
        if nn == no_pitch then Nothing else Just (Pitch.nn nn)
    | otherwise = Nothing
    where
    k = Midi.from_key key
    nn = scale_key_to_nn scale Unboxed.! k

-- ** Flag

-- | Various instrument flags.  Add new ones at the bottom to avoid messing up
-- serialization.
data Flag =
    -- | Patch doesn't pay attention to duration.  E.g., drum samples may not
    -- pay attention to note off.  The UI can use this to create zero duration
    -- events for this patch.  TODO this is actually not MIDI-specific, so it
    -- should go in Instrument.Common.
    Triggered
    -- | Patch uses continuous pressure control, assigned to CC 2 (breath),
    -- instead of trigger velocity.  This is used to support the @dyn@ control.
    -- Percussive instruments like pianos map it to MIDI velocity, and
    -- continuous instruments like winds always have maximum velocity and map
    -- @dyn@ to breath.
    | Pressure
    -- | If set, a keysitch has to be held while its note is playing.
    -- Otherwise, it will just be tapped before the note starts.
    | HoldKeyswitch
    -- | When playing from mid-score, scan backwards for the first overlapping
    -- notes with this instrument set and resume that note.  This way you can
    -- play long notes like tambura from the middle.
    | ResumePlay
    -- | If there are overlapping notes with the same MIDI key, delay all
    -- NoteOffs until the final one.  This is for synthesizers which turn the
    -- note off on the first one, such as Kontakt.
    | UseFinalNoteOff
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Pretty Flag where pretty = showt
instance Serialize.Serialize Flag where
    put = Serialize.put_enum
    get = Serialize.get_enum

add_flag :: Flag -> Set Flag -> Set Flag
add_flag = Set.insert

remove_flag :: Flag -> Set Flag -> Set Flag
remove_flag = Set.delete

-- ** InitializePatch

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch =
    -- | Send these msgs to initialize the patch.  It should be a patch
    -- change or a sysex.
    InitializeMidi ![Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage !Text
    | NoInitialization
    deriving (Eq, Ord, Show)

instance Pretty InitializePatch where
    format (InitializeMidi msgs) =
        Pretty.text "InitializeMidi" Pretty.<+> Pretty.format msgs
    format init = Pretty.text (showt init)

-- ** AttributeMap

-- | This is a specialization of 'Common.AttributeMap' for MIDI.
-- You should use a constructor like 'keyswitches', which will call
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
    | PitchedKeymap !Midi.Key !Midi.Key !Midi.Key
    deriving (Eq, Ord, Show)

instance Pretty Keymap where
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

instance Pretty Keyswitch where
    format (Keyswitch key) = "key:" <> Pretty.format key
    format (ControlSwitch cc val) =
        "cc:" <> Pretty.format cc <> "/" <> Pretty.format val
    format (Aftertouch val) = "at:" <> Pretty.format val

-- | An AttributeMap with just keyswitches.
keyswitches :: [(Attrs.Attributes, [Keyswitch])] -> AttributeMap
keyswitches attr_ks =
    Common.attribute_map [(attrs, (ks, Nothing)) | (attrs, ks) <- attr_ks]

-- | An AttributeMap with a single Midi.Key keyswitch per Attribute.
single_keyswitches :: [(Attrs.Attributes, Midi.Key)] -> AttributeMap
single_keyswitches = keyswitches . map (second ((:[]) . Keyswitch))

cc_keyswitches :: Midi.Control -> [(Attrs.Attributes, Midi.ControlValue)]
    -> AttributeMap
cc_keyswitches cc = keyswitches . map (second ((:[]) . ControlSwitch cc))

keymap :: [(Attrs.Attributes, Keymap)] -> AttributeMap
keymap table =
    Common.attribute_map [(attr, ([], Just keymap)) | (attr, keymap) <- table]

-- | An AttributeMap with just unpitched keymaps.
unpitched_keymap :: [(Attrs.Attributes, Midi.Key)] -> AttributeMap
unpitched_keymap = keymap . map (second UnpitchedKeymap)

-- | The MIDI message to activate the given Keyswitch.
keyswitch_on :: Midi.Key -> Keyswitch -> Midi.ChannelMessage
keyswitch_on midi_key ks = case ks of
    Keyswitch key -> Midi.NoteOn key 64
    ControlSwitch cc val -> Midi.ControlChange cc val
    Aftertouch val -> Midi.Aftertouch midi_key val

keyswitch_off :: Keyswitch -> Maybe Midi.ChannelMessage
keyswitch_off ks = case ks of
    Keyswitch key -> Just $ Midi.NoteOff key 64
    ControlSwitch {} -> Nothing
    Aftertouch {} -> Nothing
