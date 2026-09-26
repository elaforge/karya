-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.
-}
module Perform.Midi.Patch (
    -- * Config
    Config(..), configAddrs
    , mergeDefaults
    , allocation, controlDefaults, initialization, settings
    , config
    , Initialization(..), Addr, Voices
    , hasFlag
    -- Re-exported so instrument definitions don't have to have
    -- Midi.Control.PbRange.
    , Control.PbRange
    -- ** Settings
    , Settings(..)
    , pitchBendRange, decay, scale, flags

    -- * Patch
    , Patch(..), name, controlMap
    , initialize, attributeMap, modeMap, defaults
    , patch
    , defaultName
    -- ** Scale
    , Scale(..) -- should just be Scale(scale_name), but Cmd.Serialize
    , makeScale
    , makeScaleDense
    , convertScale, nnAt
    , scaleNns, scaleOffsets, scaleTuning
    -- ** Flag
    , Flag(..)
    , addFlag, removeFlag
    -- ** InitializePatch
    , InitializePatch(..)
    , initializeMidi
    -- ** AttributeMap
    , AttributeMap, Keymap(..), Keyswitch(..)
    , keyswitches, singleKeyswitches, ccKeyswitches, ccKeyswitchesPermute
    , keymap, unpitchedKeymap
    , keyswitchOn, keyswitchOff
    -- ** ModeMap
    , ModeMap(..)
    , makeModeMap, ccModeMap
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
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
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
data Config = Config
    -- | An instrument may have multiple addresses assigned to it, which means
    -- that it can be multiplexed across multiple channels.  In addition,
    -- multiple instruments can be allocated to overlapping addresses, which is
    -- how keyswitches work; each one is considered a separate instrument.  An
    -- instrument wishing to use an address will emit an appropriate message to
    -- configure it (probably a keyswitch, possibly a program change).
    --
    -- Each Addr has a count of how many simultaneous voices the addr can
    -- handle.  Nothing means there's no limit.
    { configAllocation :: [(Addr, Maybe Voices)]
    , configInitialization :: Maybe Initialization
    , configSettings :: Settings
    } deriving (Eq, Show, Generics.Generic)

allocation = Lens.lens configAllocation (\r a -> r { configAllocation = a })
initialization = Lens.lens configInitialization
    (\r a -> r { configInitialization = a })
settings = Lens.lens configSettings (\r a -> r { configSettings = a })

configAddrs :: Config -> [Addr]
configAddrs = map fst . configAllocation

config :: [(Addr, Maybe Voices)] -> Config
config alloc = Config
    { configAllocation = alloc
    , configInitialization = Nothing
    , configSettings = mempty
    }

mergeDefaults :: Patch -> Config -> Config
mergeDefaults patch = settings %= (<> patchDefaults patch)

instance Pretty Config where format = Pretty.formatG_

-- | Document what kinds of initialization this instrument needs.  Each
-- instrument is initialized once when the score is loaded.
data Initialization
    = Tuning -- ^ Configure tuning with 'Midi.realtime_tuning'.
    | NrpnTuning -- ^ Configure tuning with 'Midi.nrpn_tuning'.
    deriving (Show, Ord, Eq, Enum, Bounded)
instance Pretty Initialization where pretty = showt

-- | MIDI instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)
-- | Number of simultaneous voices a certain Addr supports, aka polyphony.
type Voices = Int

hasFlag :: Config -> Flag -> Bool
hasFlag config flag = maybe False (Set.member flag) (settings#flags #$ config)

-- ** Settings

-- | This has instrument configuration which has built-in defaults but can also
-- be modified per score.  When the instrument is looked up
-- (Cmd.resolve_instrument), 'patch_defaults' is merged with 'config_settings'
-- via 'merge_defaults'.
data Settings = Settings
    { configFlags :: Maybe (Set Flag)
    , configScale :: Maybe Scale
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , configDecay :: Maybe RealTime
    , configPitchBendRange :: Maybe Control.PbRange
    -- | Default controls for this instrument, will always be set unless
    -- explicitly replaced.  This hopefully avoids the problem where
    -- a synthesizer starts in an undefined state.  This is different from
    -- 'Common.config_controls' in that these are meant to provide a default
    -- for synthesizer state, so these are only applied during conversion, and
    -- thus should only contain controls the MIDI instrument understands.
    , configControlDefaults :: Maybe ScoreT.ControlValMap
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Settings where format = Pretty.formatG_

instance Semigroup Settings where
    (<>)    (Settings flags1 scale1 decay1 pbRange1 cdefaults1)
            (Settings flags2 scale2 decay2 pbRange2 cdefaults2) =
        Settings (flags1 <|> flags2) (scale1 <|> scale2) (decay1 <|> decay2)
            (pbRange1 <|> pbRange2) (cdefaults1 <|> cdefaults2)
instance Monoid Settings where
    mempty = Settings
        { configFlags = Nothing
        , configScale = Nothing
        , configDecay = Nothing
        , configPitchBendRange = Nothing
        , configControlDefaults = Nothing
        }
    mappend = (<>)

pitchBendRange = Lens.lens configPitchBendRange
    (\r a -> r { configPitchBendRange = a })
decay = Lens.lens configDecay (\r a -> r { configDecay = a })
scale = Lens.lens configScale (\r a -> r { configScale = a })
flags = Lens.lens configFlags (\r a -> r { configFlags = a })
controlDefaults = Lens.lens configControlDefaults
    (\r a -> r { configControlDefaults = a })

-- * Patch

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its
-- 'Instrument.Inst.Synth'.
data Patch = Patch
    -- | This is the name of the instrument on the synthesizer, and likely has
    -- all sorts of wacky characters in it, and may not be unique, even on
    -- a single synth.  This is just for documentation, and is not actually
    -- used by anyone, though the unique name may be derived from this one.
    --
    -- The patch's unique name, as used by 'InstT.Qualified' to look up
    -- the patch, is in 'Instrument.Inst.synth_insts'.
    { patchName :: Text
    , patchControlMap :: Control.ControlMap
    , patchInitialize :: InitializePatch
    , patchAttributeMap :: AttributeMap
    , patchModeMap :: ModeMap
    , patchDefaults :: Settings
    } deriving (Eq, Show)

instance Pretty Patch where
    format (Patch name cmap init attrMap modeMap defaults) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("control_map", Pretty.format cmap)
            , ("initialize", Pretty.format init)
            , ("attribute_map", Pretty.format attrMap)
            , ("mode_map", Pretty.format modeMap)
            , ("defaults", Pretty.format defaults)
            ]

name = Lens.lens patchName (\r a -> r { patchName = a })
controlMap = Lens.lens patchControlMap (\r a -> r { patchControlMap = a })
initialize = Lens.lens patchInitialize (\r a -> r { patchInitialize = a })
attributeMap = Lens.lens patchAttributeMap
    (\r a -> r { patchAttributeMap = a })
modeMap = Lens.lens patchModeMap (\r a -> r { patchModeMap = a })
defaults = Lens.lens patchDefaults (\r a -> r { patchDefaults = a })

-- | Create a Patch with empty vals, to set them as needed.
patch :: Control.PbRange -> InstT.Name -> Patch
patch pbRange name = Patch
    { patchName = name
    , patchControlMap = mempty
    , patchInitialize = NoInitialization
    , patchAttributeMap = Common.AttributeMap []
    , patchModeMap = ModeMap mempty
    , patchDefaults = mempty { configPitchBendRange = Just pbRange }
    }

-- | This is a convention for the default instrument of a synth.  This is
-- useful for softsynths whose patches all generally have the same config.
defaultName :: InstT.Name
defaultName = ""

-- ** Scale

{- | Describe the tuning of a MIDI patch.

    This is used both to describe a patch tuned to something other than 12TET,
    and to retune a 12TET patch.

    The Scale is used during performance to warp played pitches to the patch's
    tuning.  The idea is that they will warp to integral 'Midi.Key's that won't
    need any tuning and can thus all go on a single MIDI channel.
-}
data Scale = Scale
    { scaleName :: Text
    -- | If a patch is tuned to something other than 12TET, this vector maps
    -- MIDI key numbers to their NNs, or 'no_pitch' if the patch doesn't
    -- support that key.
    , scaleKeyToNn :: Unboxed.Vector Double
    } deriving (Eq, Ord, Show)

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

noPitch :: Double
noPitch = -1

-- | Fill in non-adjacent MIDI keys by interpolating the neighboring
-- NoteNumbers.  This is because a 0 between two notes will prevent pitch
-- slides.  Another problem is that the MIDI performer has no notion of
-- instruments that don't support certain key numbers.  That could be added
-- but it's simpler to just not have patches like that.
makeScale :: Text -> [(Midi.Key, Pitch.NoteNumber)] -> Scale
makeScale name keys = Scale
    { scaleName = name
    , scaleKeyToNn = empty Unboxed.// map convert (interpolateGaps keys)
    }
    where
    convert (k, Pitch.NoteNumber nn) = (Midi.fromKey k, nn)
    empty = Unboxed.fromList $ replicate 128 noPitch

makeScaleDense :: Text -> [Double] -> Scale
makeScaleDense scaleName keys = Scale
    { scaleName
    , scaleKeyToNn = Unboxed.fromList $
        take 128 keys <> replicate (128 - length keys) noPitch
    }

interpolateGaps :: [(Midi.Key, Pitch.NoteNumber)]
    -> [(Midi.Key, Pitch.NoteNumber)]
interpolateGaps ((k1, nn1) : rest@((k2, nn2) : _))
    | k1 + 1 == k2 = (k1, nn1) : interpolateGaps rest
    | otherwise = (k1, nn1) : map mk (Lists.range' (k1+1) k2 1)
        ++ interpolateGaps rest
    where
    mk k = (k, nn)
        where
        nn = Num.scale nn1 nn2 $ Num.normalize
            (Midi.fromKey k1) (Midi.fromKey k2) (Midi.fromKey k)
interpolateGaps xs = xs

convertScale :: Scale -> Pitch.NoteNumber -- ^ if you want this pitch
    -> Maybe Pitch.NoteNumber -- ^ play this key
convertScale (Scale _ keyToNn) (Pitch.NoteNumber nn) =
    case Util.Vector.bracket keyToNn nn of
        Just (i, low, high) | low /= noPitch ->
            Just $ Pitch.NoteNumber $ fromIntegral i + Num.normalize low high nn
        _ -> Nothing

-- *** tuning

-- | Absolute NoteNumber for each 'Midi.Key' to tune 12TET to this scale.
scaleNns :: Maybe AttributeMap -> Scale -> [(Midi.Key, Pitch.NoteNumber)]
scaleNns attrMap scale =
    [(key, nn) | (key, Just (_, nn)) <- zip [0..] (scaleTuning attrMap scale)]

-- | Relative NoteNumber offset for each 'Midi.Key' to tune 12TET to this scale.
scaleOffsets :: Maybe AttributeMap -> Scale -> [Maybe Pitch.NoteNumber]
scaleOffsets attrMap = map (fmap toOffset) . scaleTuning attrMap
    where toOffset (base, nn) = nn - Midi.fromKey base

-- | Map the mapped keys through the scale.
scaleTuning :: Maybe AttributeMap -> Scale
    -> [Maybe (Midi.Key, Pitch.NoteNumber)]
scaleTuning attrMap scale = map tuning [0..127]
    where
    tuning key
        | null ranges = (key,) <$> nnAt scale key
        | otherwise = case List.find (inRange key) ranges of
            Nothing -> Nothing
            Just (low, _, baseNn) -> (absKey,) <$> nnAt scale absKey
                where absKey = baseNn + (key - low)
    inRange key (low, high, _) = low <= key && key <= high
    ranges =
        [ (low, high, nn)
        | (_, Just (PitchedKeymap low high nn))
            <- maybe [] Common.attributeVals attrMap
        ]

nnAt :: Scale -> Midi.Key -> Maybe Pitch.NoteNumber
nnAt scale key
    | key >= 0 && k < Unboxed.length (scaleKeyToNn scale) =
        if nn == noPitch then Nothing else Just (Pitch.nn nn)
    | otherwise = Nothing
    where
    k = Midi.fromKey key
    nn = scaleKeyToNn scale Unboxed.! k

-- ** Flag

-- | Various instrument flags.  Add new ones at the bottom to avoid messing up
-- serialization.
data Flag
    -- | Patch uses continuous pressure control, assigned to CC 2 (breath),
    -- instead of trigger velocity.  This is used to support the @dyn@ control.
    -- Percussive instruments like pianos map it to MIDI velocity, and
    -- continuous instruments like winds always have maximum velocity and map
    -- @dyn@ to breath.
    = Pressure
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
    deriving (Eq, Ord, Show, Bounded, Enum)

instance Pretty Flag where pretty = showt

addFlag :: Flag -> Set Flag -> Set Flag
addFlag = Set.insert

removeFlag :: Flag -> Set Flag -> Set Flag
removeFlag = Set.delete

-- ** InitializePatch

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch
    -- | Send these msgs to initialize the patch.  It should be a patch
    -- change or a sysex.  Channel is ignored.
    = InitializeMidi [Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage Text
    | NoInitialization
    deriving (Eq, Ord, Show)

instance Pretty InitializePatch where
    format (InitializeMidi msgs) =
        Pretty.text "InitializeMidi" Pretty.<+> Pretty.format msgs
    format init = Pretty.text (showt init)

initializeMidi :: [Midi.ChannelMessage] -> InitializePatch
initializeMidi = InitializeMidi . map (Midi.ChannelMessage 0)

-- ** AttributeMap

-- | This is a specialization of 'Common.AttributeMap' for MIDI.
-- You should use a constructor like 'keyswitches', which will call
-- 'Common.sort_attributes' to make sure there are no overlaps.
type AttributeMap = Common.AttributeMap ([Keyswitch], Maybe Keymap)

-- | A Keymap corresponds to a timbre selected by MIDI key range, rather than
-- keyswitches.  Unlike a keyswitch, this doesn't change the state of the MIDI
-- channel, so multiple keymapped notes can coexist, and keymap replaces the
-- pitch of the note.
data Keymap
    -- | This ignores the event's pitch and instead emits the given MIDI key.
    -- This is appropriate for drumkit style patches, with a separate unpitched
    -- timbre on each key.
    = UnpitchedKeymap Midi.Key
    -- | The timbre is mapped over the inclusive MIDI key range from low to
    -- high, where the pitch of the low end of the range is given by the
    -- NoteNumber.  So this transposes the event's pitch and clips it to the
    -- given range.  low, high, nn
    | PitchedKeymap Midi.Key Midi.Key Midi.Key
    deriving (Eq, Ord, Show)

instance Pretty Keymap where
    pretty (UnpitchedKeymap k) = pretty k
    pretty (PitchedKeymap low high nn) = pretty low <> "--"
        <> pretty high <> "(" <> pretty nn <> ")"

-- | A Keyswitch changes the timbre of a patch, but does so in a channel-global
-- way.  So overlapping notes with different keyswitches will be split into
-- different channels, if possible.  See NOTE [midi-state].
data Keyswitch
    = Keyswitch Midi.Key
    -- | This keyswitch is triggered by a control change.
    | ControlSwitch Midi.Control Midi.ControlValue
    -- | This is like 'ControlSwitch', except send a poly aftertouch value
    -- for the note's pitch.  This allows simultaneous different notes with
    -- different articulations.
    | Aftertouch Midi.ControlValue
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
combineEmpty :: [(Attrs.Attributes, [ks])] -> [(Attrs.Attributes, [ks])]
combineEmpty attrKs
    | null empty = nonempty
    | otherwise = (mempty, concatMap snd empty) : nonempty
    where
    (empty, nonempty) = List.partition ((==mempty) . fst) attrKs

-- | An AttributeMap with a single Midi.Key keyswitch per Attribute.
singleKeyswitches :: [(Attrs.Attributes, Midi.Key)] -> AttributeMap
singleKeyswitches = keyswitches . map (second ((:[]) . Keyswitch))

-- | An AttributeMap that uses 'ControlSwitch'es.  Each CC can have attrs at
-- several values.
ccKeyswitches :: [(Midi.Control, [(Attrs.Attributes, Midi.ControlValue)])]
    -> AttributeMap
ccKeyswitches ks = keyswitches
    [ (attrs, [ControlSwitch cc val])
    | (cc, attrControls) <- ks
    , (attrs, val) <- attrControls
    ]

-- | Like 'cc_keyswitches', except that all the controls are orthogonal, so
-- every cross-control combination of attributes is valid.
ccKeyswitchesPermute
    :: [(Midi.Control, [(Attrs.Attributes, Midi.ControlValue)])] -> AttributeMap
ccKeyswitchesPermute ks =
    keyswitches $ map (first mconcat . unzip) $ Lists.cartesian
        [ [(attrs, ControlSwitch cc val) | (attrs, val) <- attrControls]
        | (cc, attrControls) <- ks
        ]

-- | An AttributeMap with just 'Keyswitch'es.
keyswitches :: [(Attrs.Attributes, [Keyswitch])] -> AttributeMap
keyswitches attrKs = Common.attributeMap
    [ (attrs, (ks, Nothing))
    | (attrs, ks) <- combineEmpty attrKs
    ]

-- | An AttributeMap with just 'Keymap's.
keymap :: [(Attrs.Attributes, Keymap)] -> AttributeMap
keymap table =
    Common.attributeMap [(attr, ([], Just keymap)) | (attr, keymap) <- table]

-- | An AttributeMap with just unpitched keymaps.
unpitchedKeymap :: [(Attrs.Attributes, Midi.Key)] -> AttributeMap
unpitchedKeymap = keymap . map (second UnpitchedKeymap)

-- | The MIDI message to activate the given Keyswitch.
keyswitchOn :: Midi.Key -> Keyswitch -> Midi.ChannelMessage
keyswitchOn midiKey ks = case ks of
    Keyswitch key -> Midi.NoteOn key 64
    ControlSwitch cc val -> Midi.ControlChange cc val
    Aftertouch val -> Midi.Aftertouch midiKey val

keyswitchOff :: Keyswitch -> Maybe Midi.ChannelMessage
keyswitchOff ks = case ks of
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

makeModeMap
    :: [(EnvKey.Key, [(Expr.MiniVal, (ScoreT.Control, Midi.ControlValue))])]
    -> ModeMap
makeModeMap =
    ModeMap . Map.fromList . Lists.mapMaybeSnd
        (envVal . map (second (second Control.cvalToVal)))
    where
    envVal [] = Nothing
    envVal modes@((_, deflt) : _) = Just ((deflt, Map.fromList modes))

-- | Construct a ModeMap that uses MIDI CC.
ccModeMap :: [(EnvKey.Key, Midi.Control, [(Expr.MiniVal, Midi.ControlValue)])]
    -> (ModeMap, [(Midi.Control, ScoreT.Control)])
ccModeMap modes = (, controls) $ makeModeMap
    [ (key, [(miniVal, (control key, cval)) | (miniVal, cval) <- vals])
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
