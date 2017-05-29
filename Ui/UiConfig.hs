-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.UiConfig where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global
import Types


-- | Miscellaneous config data.
data Config = Config {
    -- | The default namespace is used for automatically created IDs, so each
    -- project can import other projects without clashes.
    config_namespace :: !Id.Namespace
    , config_meta :: !Meta
    -- | Derivation can start from any block, but it's useful to know which
    -- block represents the entire piece.  This way, given a position on some
    -- block I can determine where in the piece it lies, if anywhere.  This is
    -- useful for playing a block in proper context, or communicating with
    -- a program with a more absolute notion of time, like a DAW.
    , config_root :: !(Maybe BlockId)

    -- | Instrument allocations.
    , config_allocations :: !Allocations
        -- TODO I'm not a big fan of this name, since it's generic and not
        -- obviously related to instruments.  However the previous name,
        -- 'aliases', was too and I somehow lived through that.  I tried
        -- 'instruments', but it seemed too easy to confuse with Score.Inst.
    , config_lilypond :: !Lilypond.Config
    , config_default :: !Default
    , config_saved_views :: !SavedViews
    -- | Locally defined code in the ky language, as parsed by
    -- 'Derive.Parse.parse_ky'.  If the ky defines a note transformer called
    -- @GLOBAL@, it will be implicitly wrapped around every derivation.
    , config_ky :: !Text
    } deriving (Eq, Show)

-- Ui.State already has a function called 'namespace'.
namespace_ = Lens.lens config_namespace
    (\f r -> r { config_namespace = f (config_namespace r) })
meta = Lens.lens config_meta
    (\f r -> r { config_meta = f (config_meta r) })
root = Lens.lens config_root
    (\f r -> r { config_root = f (config_root r) })
allocations = Lens.lens config_allocations
    (\f r -> r { config_allocations = f (config_allocations r) })
lilypond = Lens.lens config_lilypond
    (\f r -> r { config_lilypond = f (config_lilypond r) })
default_ = Lens.lens config_default
    (\f r -> r { config_default = f (config_default r) })
saved_views = Lens.lens config_saved_views
    (\f r -> r { config_saved_views = f (config_saved_views r) })
ky = Lens.lens config_ky
    (\f r -> r { config_ky = f (config_ky r) })

-- | Unwrap the newtype for convenience.
allocations_map :: Lens Config (Map Score.Instrument Allocation)
allocations_map = Lens.lens (open . config_allocations)
    (\f r -> r { config_allocations =
        Allocations $ f $ open $ config_allocations r })
    where open (Allocations a) = a

empty_config :: Config
empty_config = Config
    { config_namespace = Id.namespace "untitled"
    , config_meta = empty_meta
    , config_root = Nothing
    , config_allocations = mempty
    , config_lilypond = Lilypond.default_config
    , config_default = empty_default
    , config_saved_views = mempty
    , config_ky = ""
    }

-- | Insert an allocation into 'config_allocations' while checking it for
-- validity.
--
-- TODO Of course there's no enforcement for this.  I could get rid of the
-- lens, but there is still uncontrolled access through 'State.modify_config'.
-- On the other hand, it might not really matter, and I do use unchecked
-- modification when the backend doesn't change.
allocate :: Inst.Backend -- ^ This should correspond to the 'alloc_qualified'.
    -> Score.Instrument -> Allocation -> Allocations
    -> Either Text Allocations
allocate backend instrument alloc (Allocations allocs) =
    maybe (Right inserted) Left $ verify_allocation backend instrument alloc
    where inserted = Allocations $ Map.insert instrument alloc allocs

verify_allocation :: Inst.Backend -> Score.Instrument -> Allocation
    -> Maybe Text
verify_allocation backend instrument alloc =
    fmap (prefix<>) $ case (alloc_backend alloc, backend) of
        (Midi {}, Inst.Midi {}) -> Nothing
        (Im, Inst.Im {}) -> Nothing
        -- I can make any patch into a dummy allocation by tossing the
        -- non-common data.  This could be confusing since it does so silently,
        -- but I tried to add an explicit Dummy Inst.Backend, but modifying
        -- Cmd.Instrument.MidiInst to support declaring them got really
        -- involved and I decided it was too complicated.
        (Dummy, _) -> Nothing
        (_, backend) -> Just $ "allocation type " <> allocation_type
            <> " /= instrument type " <> backend_type backend
    where
    qualified = alloc_qualified alloc
    prefix = pretty instrument <> " from " <> pretty qualified <> ": "
    allocation_type = case alloc_backend alloc of
        Midi {} -> "midi"
        Im -> "im"
        Dummy -> "dummy"
    backend_type backend = case backend of
        Inst.Dummy -> "dummy"
        Inst.Midi {} -> "midi"
        Inst.Im {} -> "im"

newtype Allocations = Allocations (Map Score.Instrument Allocation)
    deriving (Eq, Show, Pretty, Monoid.Monoid)

-- | Make Allocations with no verification.  This should probably only be used
-- for tests, allocations from user input should use 'allocate'.
make_allocations :: [(Score.Instrument, Allocation)] -> Allocations
make_allocations = Allocations . Map.fromList

-- | This is 'make_allocations' specialized for MIDI instruments.  Like
-- 'make_allocations', it also does no verification.
midi_allocations :: [(Score.Instrument, (InstTypes.Qualified, Patch.Config))]
    -> Allocations
midi_allocations allocs = Allocations $ Map.fromList
    [ (inst, allocation qual (Midi config))
    | (inst, (qual, config)) <- allocs
    ]

modify_allocation :: Score.Instrument -> (Allocation -> Either Text Allocation)
    -> Allocations -> Either Text Allocations
modify_allocation inst modify (Allocations allocs) = do
    alloc <- justErr ("no allocation for " <> pretty inst) $
        Map.lookup inst allocs
    alloc <- modify alloc
    return $ Allocations (Map.insert inst alloc allocs)

-- | This is the root of the dynamic (per-score) instrument config.  It's
-- divided into common and backend-specific configuration.
data Allocation = Allocation {
    alloc_qualified :: !InstTypes.Qualified
    , alloc_config :: !Common.Config
    , alloc_backend :: !Backend
    } deriving (Eq, Show)

allocation :: InstTypes.Qualified -> Backend -> Allocation
allocation qualified backend = Allocation qualified Common.empty_config backend

instance Pretty Allocation where
    format (Allocation qualified config backend) = Pretty.record "Allocation"
        [ ("qualified", Pretty.format qualified)
        , ("config", Pretty.format config)
        , ("backend", Pretty.format backend)
        ]

is_im_allocation :: Allocation -> Bool
is_im_allocation alloc = case alloc_backend alloc of
    Im -> True
    _ -> False

-- | Backend-specific config.  This should match the 'Inst.Backend' of the
-- instrument in question, ensured by 'verify_allocation'.
--
-- I can't think of a way to ensure this statically, since the instrument and
-- config are saved in instrument db and score respectively, and only come
-- together when a new score is loaded.
data Backend =
    Midi !Patch.Config
    | Im
    -- | This is for instruments without a backend.  For example a paired
    -- instrument might be written as one instrument, but realized as two
    -- different ones.
    | Dummy
    deriving (Eq, Show)

instance Pretty Backend where
    format (Midi config) = Pretty.format config
    format Im = "Im"
    format Dummy = "Dummy"

midi_config :: Backend -> Maybe Patch.Config
midi_config (Midi config) = Just config
midi_config _ = Nothing

-- | Extra data that doesn't have any effect on the score.
data Meta = Meta {
    -- | The time the score was created.  This should be reset whenever
    -- the score is started, or copied from a template.
    meta_creation :: !Time.UTCTime
    -- | The last time the score was saved.  This is useful to determine which
    -- of several saves is the latest.
    , meta_last_save :: !Time.UTCTime
    , meta_notes :: !Text
    , meta_midi_performances :: !(Map BlockId MidiPerformance)
    , meta_lilypond_performances :: !(Map BlockId LilypondPerformance)
    } deriving (Eq, Read, Show)

creation = Lens.lens meta_creation
    (\f r -> r { meta_creation = f (meta_creation r) })
last_save = Lens.lens meta_last_save
    (\f r -> r { meta_last_save = f (meta_last_save r) })
notes = Lens.lens meta_notes
    (\f r -> r { meta_notes = f (meta_notes r) })
midi_performances = Lens.lens meta_midi_performances
    (\f r -> r { meta_midi_performances = f (meta_midi_performances r) })
lilypond_performances = Lens.lens meta_lilypond_performances
    (\f r -> r { meta_lilypond_performances =
        f (meta_lilypond_performances r) })

type MidiPerformance = Performance (Vector.Vector Midi.WriteMessage)
type LilypondPerformance = Performance Text

empty_meta :: Meta
empty_meta = Meta
    { meta_creation = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , meta_last_save = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , meta_notes = ""
    , meta_midi_performances = mempty
    , meta_lilypond_performances = mempty
    }

-- | A record of the last successful performance that sounded as expected.  You
-- can compare this with the current performance to see if code changes have
-- messed things up.
--
-- I'm ambivalent about including this in the save file, since it will be saved
-- and loaded all the time when it should rarely change.  But it seems like the
-- only reliable way to keep the score and performance in sync.  Besides, it
-- shouldn't actually be that large, and if it is, the git repo save should
-- only save it when 'Config' changes.  I could also split it into its own
-- file.
data Performance a = Performance {
    perf_performance :: !a
    -- | The time this performance was recorded.
    , perf_creation :: !Time.UTCTime
    -- | The sequencer's patch level.  For darcs, this should be a patch name
    -- (technically it should be a tag's name, but it doesn't matter as long as
    -- I'm the only developer).  For git, it would be the commit hash.
    , perf_patch :: !Text
    } deriving (Eq, Read, Show)

-- | Initial values for derivation.
--
-- This used to have other fields, but they were replaced by the more general
-- 'ky' and the implicit GLOBAL call.  I haven't removed tempo yet because it's
-- the only way to change the speed for tempo-less blocks, and doesn't affect
-- (or rather, is undone automatically) for integrated blocks.
data Default = Default {
    -- | A toplevel block without a tempo track will get this tempo.
    default_tempo :: !Signal.Y
    } deriving (Eq, Read, Show)

tempo = Lens.lens default_tempo
    (\f r -> r { default_tempo = f (default_tempo r) })

empty_default :: Default
empty_default = Default { default_tempo = 1 }

instance Pretty Config where
    format (Config namespace meta root allocations lily dflt saved_views ky) =
        Pretty.record "Config"
            [ ("namespace", Pretty.format namespace)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("allocations", Pretty.format allocations)
            , ("lilypond", Pretty.format lily)
            , ("default", Pretty.format dflt)
            , ("saved_views", Pretty.format saved_views)
            , ("ky", Pretty.format ky)
            ]

instance Pretty Meta where
    format (Meta creation last_save notes midi_perf lily_perf) =
        Pretty.record "Meta"
        [ ("creation", Pretty.format creation)
        , ("last save", Pretty.format last_save)
        , ("notes", Pretty.text notes)
        , ("midi performances", Pretty.format midi_perf)
        , ("lilypond performances", Pretty.format lily_perf)
        ]

instance Pretty MidiPerformance where
    format (Performance midi creation patch) = Pretty.record "MidiPerformance"
        [ ("events", Pretty.format $ Vector.length midi)
        , ("creation", Pretty.text $ pretty creation)
        , ("patch", Pretty.text patch)
        ]

instance Pretty LilypondPerformance where
    format (Performance ly creation patch) = Pretty.record "LilypondPerformance"
        [ ("lilypond lines", Pretty.format $ Text.count "\n" ly)
        , ("creation", Pretty.text $ pretty creation)
        , ("patch", Pretty.text patch)
        ]

instance Pretty Default where
    format (Default tempo) = Pretty.record "Default"
        [ ("tempo", Pretty.format tempo) ]

instance DeepSeq.NFData Default where
    rnf (Default tempo) = tempo `seq` ()

-- | This is a place to save sets of views so you can switch between them.
-- The ViewId is the one with focus.
type SavedViews = Map Text (Map ViewId Block.View, Maybe ViewId)
