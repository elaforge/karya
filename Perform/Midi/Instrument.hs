{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.
-}
module Perform.Midi.Instrument where
import Control.DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Control as Control


default_scale :: Pitch.ScaleId
default_scale = Pitch.twelve

-- | The Instrument contains all the data necessary to render
-- a Midi.Perform.Event to a midi message.  Each Event has an attached
-- Instrument.
--
-- Don't put data unnecessary to derivation in here because they are compared
-- to each other a lot when trying to merge channels.  All that stuff should go
-- into the Patch.
data Instrument = Instrument {
    inst_synth :: SynthName
    , inst_name :: InstrumentName
    , inst_keyswitch :: Maybe Keyswitch
    -- | Instrument name as given in the Score.Instrument.  It's just used
    -- to print msgs, but it should be the same to avoid confusion.
    , inst_score_name :: String
    -- | Map control names to a control number.  Some controls are shared by
    -- all midi instruments, but some instruments have special controls.
    , inst_control_map :: Control.ControlMap

    , inst_pitch_bend_range :: Control.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , inst_maybe_decay :: Maybe Double
    -- | Scale used by this instrument.  This determines what adjustments need
    -- to be made, if any, to get a frequency indicated by the pitch track.
    , inst_scale :: Pitch.ScaleId
    } deriving (Eq, Ord, Show)

instance NFData Instrument where
    -- don't bother with the rest since instruments are constructed all at once
    rnf inst = rnf (inst_score_name inst)

instrument :: SynthName -> InstrumentName -> Maybe Keyswitch
    -> Control.ControlMap -> Control.PbRange -> Instrument
instrument synth_name name keyswitch cmap pb_range =
    set_instrument_name synth_name name keyswitch
        (Instrument "" "" Nothing "" cmap pb_range Nothing default_scale)

-- | Somewhat conservative default decay which should suit most instruments.
-- 'inst_decay' will probably only rarely be explicitly set.
default_decay :: Double
default_decay = 1.0

inst_decay :: Instrument -> Double
inst_decay = maybe default_decay id . inst_maybe_decay

set_instrument_name :: SynthName -> String -> Maybe Keyswitch -> Instrument
    -> Instrument
set_instrument_name synth_name name keyswitch inst = inst
    { inst_synth = synth_name
    , inst_name = name
    , inst_keyswitch = keyswitch
    , inst_score_name = synth_name ++ "/" ++ name ++ ks_str
    }
    where
    ks_str = case keyswitch of
        Just (Keyswitch ks_name _) -> "/" ++ ks_name
        _ -> ""


-- | Per-song instrument configuration.
data Config = Config {
    -- | An instrument may have multiple addresses assigned to it, which means
    -- that it can be multiplexed across multiple channels.  In addition,
    -- multiple instruments can be allocated to overlapping addresses, which is
    -- how keyswitches work; each one is considered a separate instrument.  An
    -- instrument wishing to use an address will emit an appropriate message to
    -- configure it (probably a keyswitch, possibly a program change).
    config_alloc :: Map.Map Score.Instrument [Addr]
    } deriving (Eq, Read, Show)
config = Config . Map.fromList

-- | Midi instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)

-- | Keyswitch name and key to activate it.
data Keyswitch = Keyswitch
    { ks_name :: String
    , ks_key :: Midi.Key
    } deriving (Eq, Ord, Show, Read)

-- * instrument db types

-- When there are multiple backends, this will have to move to a more general
-- place.

-- | A Patch has information about one specific instrument.  The performance
-- 'Instrument' and MIDI config are derived from it, via its 'Synth'.
data Patch = Patch {
    -- | The Instrument is a subset of the data available in the Patch.
    -- The patch_instrument is not necessarily the same as the one eventually
    -- used in performance, because e.g. synth controls can get added in.
    patch_instrument :: Instrument
    , patch_initialize :: InitializePatch
    -- | Keyswitches available to this instrument, if any.  Each of these is
    -- considered its own instrument, like synth\/inst\/ks.  A keyswitch key may
    -- occur more than once, and a name of \"\" is used when the instrument is
    -- looked up without a keyswitch.
    , patch_keyswitches :: KeyswitchMap
    -- | Key-value pairs used to index the patch.
    , patch_tags :: [Tag]
    -- | Some free form text about the patch.
    , patch_text :: String
    } deriving (Eq, Show)

-- | Create a Patch with empty vals, to set them as needed.
patch :: Instrument -> Patch
patch inst = Patch inst NoInitialization (KeyswitchMap []) [] ""

patch_name :: Patch -> InstrumentName
patch_name = inst_name . patch_instrument

-- | A KeyswitchMap maps a set of attributes to a keyswitch and gives
-- a piority for those mapping.  For example, if {pizz} is before {cresc}, then
-- {pizz, cresc} will map to {pizz}, unless, of course, {pizz, cresc} comes
-- before either.  So if a previous attr set is a subset of a later one, the
-- later one will never be selected.  'validate_keyswithes' will check for that.
--
-- Two keyswitches with the same key will act as aliases for each other.
--
-- TODO implement qualified attributes, like cresc.fast.  Do the matching by
-- succesively stripping off trailing attributes and only try the next
-- when all permutations are exhausted.
newtype KeyswitchMap = KeyswitchMap [(Score.Attributes, Keyswitch)]
    deriving (Eq, Show)

-- | Implement attribute priorities as described in 'KeyswitchMap'.
get_keyswitch :: KeyswitchMap -> Score.Attributes -> Maybe Keyswitch
get_keyswitch (KeyswitchMap attr_ks) attrs =
    fmap snd (List.find is_subset attr_ks)
    where
    is_subset (inst_attrs, _) = Score.attrs_set inst_attrs
        `Set.isSubsetOf` Score.attrs_set attrs

keys_of :: KeyswitchMap -> Set.Set Midi.Key
keys_of (KeyswitchMap attr_ks) = Set.fromList $ map (ks_key . snd) attr_ks

-- | Make a 'KeyswitchMap' from strings.
--
-- @
-- [(\"trem+cresc\", 38)]
--      -> KeyswitchMap [({trem, cresc}, Keyswitch \"trem+cresc\" 38)]
-- @
--
-- An empty string will be the empty set keyswitch, which is used for notes
-- with no attrs.
make_keyswitches :: [(String, Midi.Key)] -> KeyswitchMap
make_keyswitches attr_keys
    | null errs = ks_map
    | otherwise = error $ "errors constructing KeyswitchMap: "
        ++ Seq.join "; " errs
    where
    ks_map = KeyswitchMap
        [(split attr, Keyswitch attr key) | (attr, key) <- attr_keys]
        where
        split = Score.attributes . filter (not.null) . Seq.split "+"
    errs = validate_keyswithes ks_map

validate_keyswithes :: KeyswitchMap -> [String]
validate_keyswithes (KeyswitchMap attr_ks) =
    Maybe.catMaybes $ zipWith check (List.inits attrs) attrs
    where
    attrs = map (Score.attrs_set . fst) attr_ks
    check prev attr = case List.find (`Set.isSubsetOf` attr) prev of
        Just other_attr -> Just $ "attr " ++ show (Set.toList attr)
            ++ " is shadowed by " ++ show (Set.toList other_attr)
        Nothing -> Nothing

type Tag = (TagKey, TagVal)
tag :: String -> String -> Tag
tag = (,)
type TagKey = String
type TagVal = String

-- | A Synth defines common features for a set of instruments, like device and
-- controls.
data Synth = Synth {
    -- | Uniquely defines the synth, and is indexed by the
    -- Instrument.inst_synth field.
    synth_name :: SynthName
    , synth_device :: Midi.WriteDevice
    -- | Often synths have a set of common controls in addition to the
    -- global midi defaults.
    , synth_control_map :: Control.ControlMap
    } deriving (Eq, Show)

synth name wdev controls =
    Synth name (Midi.WriteDevice wdev) (Control.control_map controls)

type SynthName = String
type InstrumentName = String

-- | Describe how an instrument should be initialized before it can be played.
data InitializePatch =
    -- | Send these msgs to initialize the patch.  Should be a patch change or
    -- a sysex.
    InitializeMidi [Midi.Message]
    -- | Display this msg to the user and hope they do what it says.
    | InitializeMessage String
    | NoInitialization
    deriving (Eq, Show)

patch_summary :: Patch -> String
patch_summary patch = inst_name inst ++ " -- " ++ show (patch_tags patch)
    where inst = patch_instrument patch

add_tag :: Tag -> Patch -> Patch
add_tag tag patch = patch { patch_tags = tag : patch_tags patch }
