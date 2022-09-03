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
    , merge_defaults
    , allocation, control_defaults, initialization, settings
    , config
    , Initialization(..), Addr, Voices
    , has_flag
    -- Re-exported so instrument definitions don't have to have
    -- Midi.Control.PbRange.
    , Control.PbRange
    -- ** Settings
    , Settings(..)
    , pitch_bend_range, decay, scale, flags

    -- * Patch
    , Patch(..), name, control_map
    , initialize, attribute_map, mode_map, defaults
    , patch
    , default_name
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
    , initialize_midi
    -- ** AttributeMap
    , AttributeMap, Keymap(..), Keyswitch(..)
    , keyswitches, single_keyswitches, cc_keyswitches, cc_keyswitches_permute
    , keymap, unpitched_keymap
    , keyswitch_on, keyswitch_off
    -- ** ModeMap
    , ModeMap(..)
    , make_mode_map, cc_mode_map
#ifdef TESTING
    , module Perform.Midi.Patch
#endif
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Unboxed

import qualified GHC.Generics as Generics

import qualified Util.Lens as Lens
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Vector

import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global
import           Types


-- * Config

-- | Configuration for one MIDI instrument allocation.
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
    , config_initialization :: !(Maybe Initialization)
    , config_settings :: !Settings
    } deriving (Eq, Show, Generics.Generic)

allocation = Lens.lens config_allocation
    (\f r -> r { config_allocation = f (config_allocation r) })
initialization = Lens.lens config_initialization
    (\f r -> r { config_initialization = f (config_initialization r) })
settings = Lens.lens config_settings
    (\f r -> r { config_settings = f (config_settings r) })

config_addrs :: Config -> [Addr]
config_addrs = map fst . config_allocation

config :: [(Addr, Maybe Voices)] -> Config
config alloc = Config
    { config_allocation = alloc
    , config_initialization = Nothing
    , config_settings = mempty
    }

merge_defaults :: Patch -> Config -> Config
merge_defaults patch = settings %= (<> patch_defaults patch)

instance Pretty Config where format = Pretty.formatG_

-- | Document what kinds of initialization this instrument needs.  Each
-- instrument is initialized once when the score is loaded.
data Initialization =
    Tuning -- ^ Configure tuning with 'Midi.realtime_tuning'.
    | NrpnTuning -- ^ Configure tuning with 'Midi.nrpn_tuning'.
    deriving (Show, Ord, Eq)
instance Pretty Initialization where pretty = showt

-- | MIDI instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)
-- | Number of simultaneous voices a certain Addr supports, aka polyphony.
type Voices = Int

has_flag :: Config -> Flag -> Bool
has_flag config flag = maybe False (Set.member flag) (settings#flags #$ config)

-- ** Settings

-- | This has instrument configuration which has built-in defaults but can also
-- be modified per score.  When the instrument is looked up
-- (Cmd.resolve_instrument), 'patch_defaults' is merged with 'config_settings'
-- via 'merge_defaults'.
data Settings = Settings {
    config_flags :: !(Maybe (Set Flag))
    , config_scale :: !(Maybe Scale)
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , config_decay :: !(Maybe RealTime)
    , config_pitch_bend_range :: !(Maybe Control.PbRange)
    -- | Default controls for this instrument, will always be set unless
    -- explicitly replaced.  This hopefully avoids the problem where
    -- a synthesizer starts in an undefined state.  This is different from
    -- 'Common.config_controls' in that these are meant to provide a default
    -- for synthesizer state, so these are only applied during conversion, and
    -- thus should only contain controls the MIDI instrument understands.
    , config_control_defaults :: !(Maybe ScoreT.ControlValMap)
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Settings where format = Pretty.formatG_

instance Semigroup Settings where
    (<>)    (Settings flags1 scale1 decay1 pb_range1 cdefaults1)
            (Settings flags2 scale2 decay2 pb_range2 cdefaults2) =
        Settings (flags1 <|> flags2) (scale1 <|> scale2) (decay1 <|> decay2)
            (pb_range1 <|> pb_range2) (cdefaults1 <|> cdefaults2)
instance Monoid Settings where
    mempty = Settings
        { config_flags = Nothing
        , config_scale = Nothing
        , config_decay = Nothing
        , config_pitch_bend_range = Nothing
        , config_control_defaults = Nothing
        }
    mappend = (<>)

pitch_bend_range = Lens.lens config_pitch_bend_range
    (\f r -> r { config_pitch_bend_range = f (config_pitch_bend_range r) })
decay = Lens.lens config_decay
    (\f r -> r { config_decay = f (config_decay r) })
scale = Lens.lens config_scale
    (\f r -> r { config_scale = f (config_scale r) })
flags = Lens.lens config_flags
    (\f r -> r { config_flags = f (config_flags r) })
control_defaults = Lens.lens config_control_defaults
    (\f r -> r { config_control_defaults = f (config_control_defaults r) })

-- * Patch

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its
-- 'Instrument.Inst.Synth'.
data Patch = Patch {
    -- | This is the name of the instrument on the synthesizer, and likely has
    -- all sorts of wacky characters in it, and may not be unique, even on
    -- a single synth.  This is just for documentation, and is not actually
    -- used by anyone, though the unique name may be derived from this one.
    --
    -- The patch's unique name, as used by 'InstT.Qualified' to look up
    -- the patch, is in 'Instrument.Inst.synth_insts'.
    patch_name :: !Text
    , patch_control_map :: !Control.ControlMap
    , patch_initialize :: !InitializePatch
    , patch_attribute_map :: !AttributeMap
    , patch_mode_map :: !ModeMap
    , patch_defaults :: !Settings
    } deriving (Eq, Show)

instance Pretty Patch where
    format (Patch name cmap init attr_map mode_map defaults) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("control_map", Pretty.format cmap)
            , ("initialize", Pretty.format init)
            , ("attribute_map", Pretty.format attr_map)
            , ("mode_map", Pretty.format mode_map)
            , ("defaults", Pretty.format defaults)
            ]

name = Lens.lens patch_name (\f r -> r { patch_name = f (patch_name r) })
control_map = Lens.lens patch_control_map
    (\f r -> r { patch_control_map = f (patch_control_map r) })
initialize = Lens.lens patch_initialize
    (\f r -> r { patch_initialize = f (patch_initialize r) })
attribute_map = Lens.lens patch_attribute_map
    (\f r -> r { patch_attribute_map = f (patch_attribute_map r) })
mode_map = Lens.lens patch_mode_map
    (\f r -> r { patch_mode_map = f (patch_mode_map r) })
defaults = Lens.lens patch_defaults
    (\f r -> r { patch_defaults = f (patch_defaults r) })

-- | Create a Patch with empty vals, to set them as needed.
patch :: Control.PbRange -> InstT.Name -> Patch
patch pb_range name = Patch
    { patch_name = name
    , patch_control_map = mempty
    , patch_initialize = NoInitialization
    , patch_attribute_map = Common.AttributeMap []
    , patch_mode_map = ModeMap mempty
    , patch_defaults = mempty { config_pitch_bend_range = Just pb_range }
    }

-- | This is a convention for the default instrument of a synth.  This is
-- useful for softsynths whose patches all generally have the same config.
default_name :: InstT.Name
default_name = ""

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
    } deriving (Eq, Show)

instance Pretty Scale where
    format (Scale name _) = Pretty.constructor "Patch.Scale" [Pretty.text name]
    -- key_to_nn is kind of noisy and usually not interesting
    -- format (Scale name key_to_nn) = Pretty.record "Patch.Scale"
    --     [ ("name", Pretty.format name)
    --     , ("key_to_nn", Pretty.format $
    --         List.dropWhileEnd (== -1) $ dropWhile (== -1) $
    --         Unboxed.toList key_to_nn)
    --     ]
    --     stripped xs =
    --         where ys = List.dropWhileEnd (== -1) $ dropWhile (== -1) xs

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
    -- | Patch uses continuous pressure control, assigned to CC 2 (breath),
    -- instead of trigger velocity.  This is used to support the @dyn@ control.
    -- Percussive instruments like pianos map it to MIDI velocity, and
    -- continuous instruments like winds always have maximum velocity and map
    -- @dyn@ to breath.
    Pressure
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
    --
    -- TODO: this is disabled, remove it entirely if I really don't seem to
    -- need it.  'Perform.Midi.Perform.avoid_overlaps'.
    | UseFinalNoteOff
    -- | Obsolete, don't use.
    | Old_Triggered
    deriving (Eq, Ord, Show, Bounded, Enum)

instance Pretty Flag where pretty = showt

add_flag :: Flag -> Set Flag -> Set Flag
add_flag = Set.insert

remove_flag :: Flag -> Set Flag -> Set Flag
remove_flag = Set.delete

-- ** InitializePatch

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch =
    -- | Send these msgs to initialize the patch.  It should be a patch
    -- change or a sysex.  Channel is ignored.
    InitializeMidi ![Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage !Text
    | NoInitialization
    deriving (Eq, Ord, Show)

instance Pretty InitializePatch where
    format (InitializeMidi msgs) =
        Pretty.text "InitializeMidi" Pretty.<+> Pretty.format msgs
    format init = Pretty.text (showt init)

initialize_midi :: [Midi.ChannelMessage] -> InitializePatch
initialize_midi = InitializeMidi . map (Midi.ChannelMessage 0)

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
    -- given range.  low, high, nn
    | PitchedKeymap !Midi.Key !Midi.Key !Midi.Key
    deriving (Eq, Ord, Show)

instance Pretty Keymap where
    pretty (UnpitchedKeymap k) = pretty k
    pretty (PitchedKeymap low high nn) = pretty low <> "--"
        <> pretty high <> "(" <> pretty nn <> ")"

-- | A Keyswitch changes the timbre of a patch, but does so in a channel-global
-- way.  So overlapping notes with different keyswitches will be split into
-- different channels, if possible.  See NOTE [midi-state].
data Keyswitch =
    Keyswitch !Midi.Key
    -- | This keyswitch is triggered by a control change.
    | ControlSwitch !Midi.Control !Midi.ControlValue
    -- | This is like 'ControlSwitch', except send a poly aftertouch value
    -- for the note's pitch.  This allows simultaneous different notes with
    -- different articulations.
    | Aftertouch !Midi.ControlValue
    deriving (Eq, Ord, Show)

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

-- | Merge keyswitches for the mempty attrs.  This is because a patch may have
-- several kinds of state, which need to be undone to go back to the default
-- state.  There's less reason to overlap for non-empty attrs.
combine_empty :: [(Attrs.Attributes, [ks])] -> [(Attrs.Attributes, [ks])]
combine_empty attr_ks
    | null empty = nonempty
    | otherwise = (mempty, concatMap snd empty) : nonempty
    where
    (empty, nonempty) = List.partition ((==mempty) . fst) attr_ks

-- | An AttributeMap with a single Midi.Key keyswitch per Attribute.
single_keyswitches :: [(Attrs.Attributes, Midi.Key)] -> AttributeMap
single_keyswitches = keyswitches . map (second ((:[]) . Keyswitch))

-- | An AttributeMap that uses 'ControlSwitch'es.  Each CC can have attrs at
-- several values.
cc_keyswitches :: [(Midi.Control, [(Attrs.Attributes, Midi.ControlValue)])]
    -> AttributeMap
cc_keyswitches ks = keyswitches
    [ (attrs, [ControlSwitch cc val])
    | (cc, attr_controls) <- ks
    , (attrs, val) <- attr_controls
    ]

-- | Like 'cc_keyswitches', except that all the controls are orthogonal, so
-- every cross-control combination of attributes is valid.
cc_keyswitches_permute
    :: [(Midi.Control, [(Attrs.Attributes, Midi.ControlValue)])] -> AttributeMap
cc_keyswitches_permute ks =
    keyswitches $ map (first mconcat . unzip) $ Seq.cartesian
        [ [(attrs, ControlSwitch cc val) | (attrs, val) <- attr_controls]
        | (cc, attr_controls) <- ks
        ]

-- | An AttributeMap with just 'Keyswitch'es.
keyswitches :: [(Attrs.Attributes, [Keyswitch])] -> AttributeMap
keyswitches attr_ks = Common.attribute_map
    [ (attrs, (ks, Nothing))
    | (attrs, ks) <- combine_empty attr_ks
    ]

-- | An AttributeMap with just 'Keymap's.
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

-- ** ModeMap

-- | The ModeMap is like the 'AttributeMap', but it's triggered by the
-- event Environ, rather than Attributes.  This is suitable for modes which
-- have mutually exclusive settings.  See NOTE [midi-state].
newtype ModeMap =
    -- map Key to (default, val_to_switch)
    ModeMap (Map EnvKey.Key
        ((ScoreT.Control, Signal.Y),
            Map Expr.MiniVal (ScoreT.Control, Signal.Y)))
    deriving (Eq, Show, Pretty)

make_mode_map
    :: [(EnvKey.Key, [(Expr.MiniVal, (ScoreT.Control, Midi.ControlValue))])]
    -> ModeMap
make_mode_map =
    ModeMap . Map.fromList . Seq.map_maybe_snd
        (env_val . map (second (second Control.cval_to_val)))
    where
    env_val [] = Nothing
    env_val modes@((_, deflt) : _) = Just ((deflt, Map.fromList modes))

-- | Construct a ModeMap that uses MIDI CC.
cc_mode_map :: [(EnvKey.Key, Midi.Control, [(Expr.MiniVal, Midi.ControlValue)])]
    -> (ModeMap, [(Midi.Control, ScoreT.Control)])
cc_mode_map modes = (, controls) $ make_mode_map
    [ (key, [(mini_val, (control key, cval)) | (mini_val, cval) <- vals])
    | (key, _, vals) <- modes
    ]
    where
    controls = [(cc, control key) | (key, cc, _) <- modes]
    control = ScoreT.Control

{- NOTE [midi-state]
    notes:
      . There are several mechanisms to set state: controls, Patch.Keyswitch,
        Patch.ControlSwitch, Patch.Aftertouch, and Patch.ModeMap.
        Patch.control_defaults is the way to initialize state for controls,
        while Keyswitch and (I think) Aftertouch is handled by the keyswitch
        mechanism in Midi.Perform.
      . The problem is that I have to manually put ModeMap and ControlSwitch
        in control_defaults, otherwise the channel gets stuck in the last state.
        E.g. Patch.cc_mode_map has to return controls and defaults.
      . Also ControlSwitch keyswitches are no longer handled like other
        keyswitches, but are merged with controls in Midi.Convert.
      . Historically, keyswitches were just keyswitches, which are exclusive.
        But ControlSwitches were introduced to be non-exclusive.
    other issues:
      / I think cc keyswitches should combine by default, so I don't need
        Patch.cc_keyswitches_permute.
        . Maybe they shouldn't be in the AttributeMap then?  Keymaps are also in
          the AttributeMap, and those are exclusive.
        . Keymaps are mostly for percussion strokes, but maybe Attributes are
          not really suited because strokes are exclusive... except true
          modifiers like variations.
        . Patch.Aftertouch is used by Reyong, for exclusive: cek, mute, open.
        . Patch.ControlSwitch used by:
          CUtil.make_cc_keymap, used by mridangam and pakhawaj, which tret them
          exclusively.
        . Ok, so maybe all other uses are actually exclusive.  I can go with
          permutations for now, but if I wind up with more, then I would have
          a separate "inclusive" attribute map.
        . This only applies to MIDI, and not many MIDI instruments are that
          expressive.  If I do my own instruments, it'll be in im, and so
          I can do inclusive attributes or controls.
      . >pianoteq/harp could use Patch.cc_keyswitches_permute
      . I wanted to change Keyswitch to Midi.Key in MIDI Event, but aftertouch
        has no control equivalent.
      . What if I just got rid of Patch.ControlSwitch?  The only other use is
        kendang and mridangam switches, but maybe those are legit?  Why don't
        I have issues resetting mridangam strokes?
        . Maybe I do!  The fact that I get the wrong kin/tan implies that
          they're not being set for thru at least.
      . Maybe I just don't use them in swam.  I can use postproc to turn attrs
        into controls.
      . It's awkward how Patch.ControlSwitch is in low level cc num and val,
        because I have to convert to Score.Control and back again.
        . Also it's error prone how I have to both assign controls and defaults.
        . Can't I put in default modes as defaults just like keyswitches?
-}
