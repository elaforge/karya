{- | Description of a midi-specific instrument, as well as the runtime midi
    device and channel mapping.

    This is all a little too complicated.

    The complete description for a set of instruments is a 'MidiDb.SynthDesc'.
    This is just a (Synth, PatchMap) pair, and a PatchMap is a map from
    instrument name to Patch.  A Patch contains an Instrument, which is the
    subset of data needed for performance.  Since there is a separate
    Instrument per keyswitch, multiple Instruments may be generated from
    a single Patch.  Patches also inherit some information from their Synth.
    So the hierarchy, from general to specific, goes
    @Synth -> Patch -> Instrument@.

    Creation of a SynthDesc is a little complicated because of the
    inter-relationships between the types.  A Patch is created with an
    Instrument as a template, but the template Instrument also wants know
    the synth name for error reporting, so those should be kept in sync.
-}
module Perform.Midi.Instrument where
import Control.DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Control as Control


default_scale :: Pitch.ScaleId
default_scale = Pitch.twelve

-- * instrument

-- | The Instrument contains all the data necessary to render
-- a Midi.Perform.Event to a midi message.  Each Event has an attached
-- Instrument.
--
-- Don't put data unnecessary to derivation in here because they are compared
-- to each other a lot when trying to merge channels.  All that stuff should go
-- into the Patch.  TODO if it helps performance, have a separate
-- Perform.Instrument that includes a fingerprint for fast comparison.
data Instrument = Instrument {
    -- | For wildcard patches, the name should be left blank and will be filled
    -- in by however the instrument is looked up.  This doesn't have the synth
    -- on it, so it may not be unique.
    inst_name :: InstrumentName

    -- | 'inst_score', 'inst_synth', and 'inst_keyswitch' are
    -- automatically filled with data from the Synth and Patch.  They should
    -- be left empty in the 'patch_instrument' template instance.
    --
    -- 'inst_score' should uniquely name one instrument in a certain
    -- performance, but I can't think about whether that's guaranteed at the
    -- moment.  TODO think more
    , inst_score :: Score.Instrument
    , inst_synth :: SynthName
    , inst_keyswitch :: Maybe Keyswitch

    -- | Some midi instruments, like drum kits, have a different sound on each
    -- key.  If there is a match in this map, the pitch will be replaced with
    -- the given key.
    , inst_keymap :: Map.Map Score.Attributes Midi.Key
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
    rnf inst = rnf (inst_score inst)

instance Pretty.Pretty Instrument where
    pretty inst = '>' : Score.inst_name (inst_score inst)

-- ** construction

-- These functions are the external interface for creating Instruments.  It's
-- important to have a somewhat abstract interface because lots of instruments
-- are created by hand in the Local.Instrument hierarchy.  If everyone uses the
-- functions in here I can hopefully provide some insulation against changes
-- in the underlying type.

-- | Initialize with values I think just about every instrument will want to
-- set.  The rest can be initialized with set_* functions.
instrument :: InstrumentName -> [(Midi.Control, String)] -> Control.PbRange
    -> Instrument
instrument name cmap pb_range = Instrument {
    inst_name = name
    , inst_score = Score.Instrument ""
    , inst_synth = ""
    , inst_keyswitch = Nothing
    , inst_keymap = Map.empty
    , inst_control_map = Control.control_map cmap
    , inst_pitch_bend_range = pb_range
    , inst_maybe_decay = Nothing
    , inst_scale = default_scale
    }

-- | A wildcard instrument has its name automatically filled in, and has no
-- need for controls since they can go in the Synth.
wildcard_instrument :: Control.PbRange -> Instrument
wildcard_instrument = instrument "*" []
    -- The inst name will be replaced, but 'MidiDb.validate' at least can use
    -- the template name when reporting errors.

-- ** defaults

-- | Somewhat conservative default decay which should suit most instruments.
-- 'inst_decay' will probably only rarely be explicitly set.
default_decay :: Double
default_decay = 1.0

inst_decay :: Instrument -> Double
inst_decay = maybe default_decay id . inst_maybe_decay

-- * config

-- | Per-score instrument configuration.
data Config = Config {
    -- | An instrument may have multiple addresses assigned to it, which means
    -- that it can be multiplexed across multiple channels.  In addition,
    -- multiple instruments can be allocated to overlapping addresses, which is
    -- how keyswitches work; each one is considered a separate instrument.  An
    -- instrument wishing to use an address will emit an appropriate message to
    -- configure it (probably a keyswitch, possibly a program change).
    config_alloc :: Map.Map Score.Instrument [Addr]
    } deriving (Eq, Read, Show)

config :: [(Score.Instrument, [Addr])] -> Config
config = Config . Map.fromList

empty_config :: Config
empty_config = config []

-- | Midi instruments are addressed by a (device, channel) pair, allocated in
-- 'Config'.
type Addr = (Midi.WriteDevice, Midi.Channel)

-- | Key to activate a keyswitch.
newtype Keyswitch = Keyswitch { ks_key :: Midi.Key }
    deriving (Eq, Ord, Show, Read)

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
    , patch_flags :: Set.Set Flag
    , patch_initialize :: InitializePatch
    -- | Keyswitches available to this instrument, if any.  Each of these is
    -- considered its own instrument, like synth\/inst\/ks.  A keyswitch key
    -- may occur more than once, and a name of \"\" is used when the
    -- instrument is looked up without a keyswitch.
    , patch_keyswitches :: KeyswitchMap
    -- | Key-value pairs used to index the patch.
    , patch_tags :: [Tag]
    -- | Some free form text about the patch.
    , patch_text :: String
    -- | The patch was read from this file.
    , patch_file :: FilePath
    } deriving (Eq, Show)

-- | Create a Patch with empty vals, to set them as needed.
patch :: Instrument -> Patch
patch inst = Patch inst Set.empty NoInitialization (KeyswitchMap []) [] "" ""

patch_name :: Patch -> InstrumentName
patch_name = inst_name . patch_instrument

set_keyswitches :: [(Score.Attributes, Midi.Key)] -> Patch -> Patch
set_keyswitches ks patch = patch { patch_keyswitches = keyswitch_map ks }

set_keymap :: [(Score.Attributes, Midi.Key)] -> Patch -> Patch
set_keymap kmap patch = patch { patch_instrument = (patch_instrument patch)
    { inst_keymap = Map.fromList kmap } }

set_flag :: Flag -> Patch -> Patch
set_flag flag patch =
    patch { patch_flags = Set.insert flag (patch_flags patch) }

has_flag :: Flag -> Patch -> Bool
has_flag flag = Set.member flag . patch_flags

-- | Various instrument flags.
data Flag =
    -- | Patch doesn't pay attention to duration.  E.g., drum samples may not
    -- pay attention to note off.  The UI can use this to create zero duration
    -- events for this patch.
    Triggered
    -- | Patch uses continuous pressure control, instead of trigger velocity.
    -- This is used to support the @p@ control.  Percussive instruments like
    -- pianos map it to MIDI velocity, and continuous instruments like winds
    -- always have maximum velocity and max @p@ to breath.
    | Pressure
    deriving (Eq, Ord, Show)

-- | A KeyswitchMap maps a set of attributes to a keyswitch and gives
-- a piority for those mapping.  For example, if {pizz} is before {cresc}, then
-- {pizz, cresc} will map to {pizz}, unless, of course, {pizz, cresc} comes
-- before either.  So if a previous attr set is a subset of a later one, the
-- later one will never be selected.  'overlapping_keyswitches' will check for
-- that.
--
-- Two keyswitches with the same key will act as aliases for each other.
--
-- TODO implement qualified attributes, like cresc.fast.  Do the matching by
-- succesively stripping off trailing attributes and only try the next
-- when all permutations are exhausted.
newtype KeyswitchMap = KeyswitchMap [(Score.Attributes, Keyswitch)]
    deriving (Eq, Show)

-- | Make a 'KeyswitchMap'.
--
-- An empty string will be the empty set keyswitch, which is used for notes
-- with no attrs.
keyswitch_map :: [(Score.Attributes, Midi.Key)] -> KeyswitchMap
keyswitch_map attr_keys =
    KeyswitchMap [(attrs, Keyswitch key) | (attrs, key) <- attr_keys]

overlapping_keyswitches :: KeyswitchMap -> [String]
overlapping_keyswitches (KeyswitchMap attr_ks) =
    Maybe.catMaybes $ zipWith check (List.inits attrs) attrs
    where
    attrs = map (Score.attrs_set . fst) attr_ks
    check prev attr = case List.find (`Set.isSubsetOf` attr) prev of
        Just other_attr -> Just $ "keyswitch attrs "
            ++ Pretty.pretty (Score.Attributes attr) ++ " shadowed by "
            ++ Pretty.pretty (Score.Attributes other_attr)
        Nothing -> Nothing

-- | Implement attribute priorities as described in 'KeyswitchMap'.  Return
-- the attributes matched in addition to the Keyswitch.
get_keyswitch :: KeyswitchMap -> Score.Attributes
    -> Maybe (Keyswitch, Score.Attributes)
get_keyswitch (KeyswitchMap attr_ks) attrs =
    fmap (uncurry (flip (,))) (List.find is_subset attr_ks)
    where
    is_subset (inst_attrs, _) = Score.attrs_set inst_attrs
        `Set.isSubsetOf` Score.attrs_set attrs

keys_of :: KeyswitchMap -> Set.Set Midi.Key
keys_of (KeyswitchMap attr_ks) = Set.fromList $ map (ks_key . snd) attr_ks

type Tag = (TagKey, TagVal)
tag :: String -> String -> Tag
tag = (,)
type TagKey = String
type TagVal = String

-- | A Synth defines common features for a set of instruments, like device and
-- controls.  Synths form a global flat namespace and must be unique.  They
-- have abbreviated names because they prefix the instrument name, which has
-- to be written in the score.
data Synth = Synth {
    -- | Uniquely defines the synth.
    synth_name :: SynthName
    -- | Instruments are allocated to 'Addr's in the 'Config', but since synth
    -- device associations are sometimes static (especially for hardware
    -- synths), it can be useful to have a hardcoded default.
    , synth_device :: Maybe Midi.WriteDevice
    -- | Often synths have a set of common controls in addition to the
    -- global midi defaults.
    , synth_control_map :: Control.ControlMap
    } deriving (Eq, Show)

synth :: SynthName -> [(Midi.Control, String)] -> Synth
synth name controls = Synth name Nothing (Control.control_map controls)

set_device :: String -> Synth -> Synth
set_device dev synth = synth { synth_device = Just (Midi.WriteDevice dev) }

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
    deriving (Eq, Ord, Show)

patch_summary :: Patch -> String
patch_summary patch = inst_name inst ++ " -- " ++ show (patch_tags patch)
    where inst = patch_instrument patch

add_tag :: Tag -> Patch -> Patch
add_tag tag patch = patch { patch_tags = tag : patch_tags patch }


-- | Constructor for a softsynth with a single wildcard patch.  Used by
-- 'Instrument.MidiDb.softsynth'.
make_softsynth :: SynthName -> Maybe String -> Control.PbRange
    -> [(Midi.Control, String)] -> (Synth, Patch)
make_softsynth name device pb_range controls = (synth, template_patch)
    where
    synth = Synth name (fmap Midi.WriteDevice device)
        (Control.control_map controls)
    template_patch = patch (wildcard_instrument pb_range)
