-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.UiConfig (
    Config(..)
    , empty_config
    , namespace_, meta, root, allocations, lilypond, default_, saved_views
    , ky, tscore
    , allocations_map
    , allocate
    , verify_allocation
    , Allocations(..), unallocations
    , make_allocations
    , midi_allocations
    , modify_allocation
    , Allocation(..)
    , allocation
    , has_im, has_midi, has_sc
    , is_im_allocation, is_midi_allocation
    , play_cache

    , Backend(..), backend_name
    , midi_config
    , convert_backend
    , Meta(..)
    , empty_meta
    , creation, last_save, notes, midi_performances, lilypond_performances
    , im_performances
    , MidiPerformance, LilypondPerformance, ImPerformance
    , Performance(..)
    , make_performance
    , Default(..)
    , tempo
    , SavedViews
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified GHC.Generics as Generics

import qualified Util.Lens as Lens
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.SourceControl as SourceControl

import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal

import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.Block as Block
import qualified Ui.Id as Id

import           Global
import           Types


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
        -- 'instruments', but it seemed too easy to confuse with
        -- 'ScoreT.Instrument'.
    , config_lilypond :: !Lilypond.Config
    , config_default :: !Default
    , config_saved_views :: !SavedViews
    -- | Locally defined code in the ky language, as parsed by
    -- 'Derive.Parse.parse_ky'.  If the ky defines a note transformer called
    -- @GLOBAL@, it will be implicitly wrapped around every derivation.
    , config_ky :: !Text
    , config_tscore :: !Text
    } deriving (Eq, Show)

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
    , config_tscore = ""
    }

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
tscore = Lens.lens config_tscore
    (\f r -> r { config_tscore = f (config_tscore r) })

-- | Unwrap the newtype for convenience.
allocations_map :: Lens Config (Map ScoreT.Instrument Allocation)
allocations_map = Lens.lens (open . config_allocations)
    (\f r -> r { config_allocations =
        Allocations $ f $ open $ config_allocations r })
    where open (Allocations a) = a

-- | Insert an allocation into 'config_allocations' while checking it for
-- validity.
--
-- TODO Of course there's no enforcement for this.  I could get rid of the
-- lens, but there is still uncontrolled access through 'State.modify_config'.
-- On the other hand, it might not really matter, and I do use unchecked
-- modification when the backend doesn't change.
allocate :: Inst.Backend -- ^ This should the result of looking up
    -- 'alloc_qualified' in the instrument db.
    -> ScoreT.Instrument -> Allocation -> Allocations
    -> Either Text Allocations
allocate backend instrument alloc (Allocations allocs) =
    maybe (Right inserted) Left $
        verify_allocation (Allocations allocs) backend instrument alloc
    where inserted = Allocations $ Map.insert instrument alloc allocs

verify_allocation :: Allocations -> Inst.Backend -> ScoreT.Instrument
    -> Allocation -> Maybe Text
verify_allocation allocs backend instrument alloc =
    fmap (prefix<>) $
        verify_backends_match backend alloc
        <|> verify_no_overlapping_addrs allocs alloc instrument
    where
    prefix = pretty instrument <> " from " <> pretty qualified <> ": "
    qualified = alloc_qualified alloc

verify_no_overlapping_addrs :: Allocations -> Allocation
    -> ScoreT.Instrument -> Maybe Text
verify_no_overlapping_addrs (Allocations allocs) alloc instrument
    | not (null out_of_range) =
        Just $ "invalid MIDI channel: " <> pretty out_of_range
    | null overlaps = Nothing
    | otherwise = Just $ "instruments with overlapping channel allocations: "
        <> Text.intercalate ", "
            [ pretty addr <> " used by " <> pretty inst
            | (addr, inst) <- overlaps
            ]
    where
    out_of_range = filter (not . Num.inRange 0 16 . snd) $ addrs_of alloc
    overlaps = mapMaybe find (addrs_of alloc)
    find addr = (addr,) . fst <$>
        List.find ((addr `elem`) . addrs_of . snd)
            (filter ((/=instrument) . fst) (Map.toList allocs))
            -- Don't count this instrument as an overlap, since I'll be
            -- replacing it.
    addrs_of alloc = case alloc_backend alloc of
        Midi config -> map fst (Patch.config_allocation config)
        _ -> []

verify_backends_match :: Inst.Backend -> Allocation -> Maybe Text
verify_backends_match backend alloc = case (alloc_backend alloc, backend) of
    (Midi {}, Inst.Midi {}) -> Nothing
    (Im, Inst.Im {}) -> Nothing
    (Sc, Inst.Sc {}) -> Nothing
    (Dummy {}, Inst.Dummy {}) -> Nothing
    _ -> Just $ "allocation type " <> backend_name (alloc_backend alloc)
        <> " /= instrument type " <> Inst.backend_name backend

newtype Allocations = Allocations (Map ScoreT.Instrument Allocation)
    deriving (Eq, Show, Pretty, Semigroup, Monoid)

unallocations :: Allocations -> Map ScoreT.Instrument Allocation
unallocations (Allocations m) = m

-- | Make Allocations with no verification.  This should probably only be used
-- for tests, allocations from user input should use 'allocate'.
make_allocations :: [(ScoreT.Instrument, Allocation)] -> Allocations
make_allocations = Allocations . Map.fromList

-- | This is 'make_allocations' specialized for MIDI instruments.  Like
-- 'make_allocations', it also does no verification.
midi_allocations :: [(ScoreT.Instrument, (InstT.Qualified, Patch.Config))]
    -> Allocations
midi_allocations allocs = Allocations $ Map.fromList
    [ (inst, allocation qual (Midi config))
    | (inst, (qual, config)) <- allocs
    ]

modify_allocation :: ScoreT.Instrument -> (Allocation -> Either Text Allocation)
    -> Allocations -> Either Text Allocations
modify_allocation instrument modify (Allocations allocs) = do
    alloc <- justErr ("no allocation for " <> pretty instrument) $
        Map.lookup instrument allocs
    new_alloc <- modify alloc
    unless (same_backend (alloc_backend alloc) (alloc_backend new_alloc)) $
        Left "modify_allocation changed the backend"
    return $ Allocations $ Map.insert instrument new_alloc allocs

{- | This is the root of the dynamic (per-score) instrument config.  It's
    divided into common and backend-specific configuration.

    How instruments work:

    The terminology is a bit inconsistent, but the intention is:

    'Inst.Synth' - Container for Patches.

    Patch - Statically declared as haskell source, contains backend-specific
    configuration, as well as common config in 'Common.Common'.  They are
    grouped with the unfortunately named 'Inst.Inst', and the backend is
    'Inst.Backend'.  They all have a unique name which is 'InstT.Qualified'
    and looks like "synth/patch-name".

    Allocation - An instantiation of a Patch in a particular score, and
    associates it with an Instrument.  Like Patch, it also has common
    config in 'Common.Config' and backend-specific config in
    'Ui.UiConfig.Backend'.  Backend-specific config may be midi devices and
    channels for midi, and Common.Config can override settings from the Patch's
    'Common.Common'.  E.g. allocate "vln1" to "vsl/solo-violin" on MIDI chan 1.

    Instrument - The is a bit overloaded, but generally should mean
    'ScoreT.Instrument', which is just a string used to look up an Allocation.

    Both the Patch and Allocation have Backends and they should match, but this
    can't be statically ensured because Patch is statically declared in the
    source while Allocation is dynamic data which is saved to and loaded from
    the score files.  'verify_backends_match' will check on allocation and
    'Cmd.Cmd.resolve_instrument' will crash if it notices mismatched backends
    don't match.

    There is an additional Dummy backend.  This is for instruments which are
    more abstract and don't correspond to a single Patch, but they can still
    have Patch level config such as special notation or env vars.  For
    instance, pemade or gangsa can refer to a whole section, and must be
    expanded into specific instruments at the derive level.  You can either
    allocate a Dummy from a Patch with 'Inst.Dummy' backend, or allocate one
    from InstT.dummy, which will resolve to an empty Patch.
-}
data Allocation = Allocation {
    alloc_qualified :: !InstT.Qualified
    , alloc_config :: !Common.Config
    , alloc_backend :: !Backend
    } deriving (Eq, Show)

allocation :: InstT.Qualified -> Backend -> Allocation
allocation qualified backend = Allocation
    { alloc_qualified = qualified
    , alloc_config = Common.empty_config
    , alloc_backend = backend
    }

instance Pretty Allocation where
    format (Allocation qualified config backend) = Pretty.record "Allocation"
        [ ("qualified", Pretty.format qualified)
        , ("config", Pretty.format config)
        , ("backend", Pretty.format backend)
        ]

has_im :: Allocations -> Bool
has_im = any is_im_allocation . Map.elems . unallocations

has_midi :: Allocations -> Bool
has_midi = any is_midi_allocation
    . filter ((/= play_cache) . alloc_qualified) . Map.elems . unallocations

has_sc :: Allocations -> Bool
has_sc = any is_sc_allocation . Map.elems . unallocations

play_cache :: InstT.Qualified
play_cache = InstT.Qualified "play-cache" ""

is_im_allocation :: Allocation -> Bool
is_im_allocation alloc = case alloc_backend alloc of
    Im -> True
    _ -> False

is_midi_allocation :: Allocation -> Bool
is_midi_allocation alloc = case alloc_backend alloc of
    Midi {} -> True
    _ -> False

is_sc_allocation :: Allocation -> Bool
is_sc_allocation alloc = case alloc_backend alloc of
    Sc -> True
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
    | Sc
    -- | This is for instruments without a backend.  For example a paired
    -- instrument might be written as one instrument, but realized as two
    -- different ones.  It should be resolved to concrete instruments during
    -- derivation, and includes an error msg show if that doesn't happen.
    -- If it's "", inherit the msg from its 'Inst.Dummy', if there is one.
    | Dummy !Text
    deriving (Eq, Show)

instance Pretty Backend where
    format = \case
        Midi config -> Pretty.format config
        Im -> "Im"
        Sc -> "Sc"
        Dummy msg -> "Dummy \"" <> Pretty.text msg <> "\""

-- | Local 'Backend' version of 'Inst.backend_name', keep them consistent.
backend_name :: Backend -> Text
backend_name = \case
    Midi {} -> "midi"
    Im -> "音"
    Sc -> "sc"
    Dummy {} -> "dummy"

same_backend :: Backend -> Backend -> Bool
same_backend b1 b2 = case (b1, b2) of
    (Midi {}, Midi {}) -> True
    (Im, Im) -> True
    (Sc, Sc) -> True
    (Dummy {}, Dummy {}) -> True
    _ -> False

midi_config :: Backend -> Maybe Patch.Config
midi_config (Midi config) = Just config
midi_config _ = Nothing

convert_backend :: Inst.Backend -> Backend
convert_backend = \case
    Inst.Dummy {} -> Dummy ""
    Inst.Midi {} -> Midi $ Patch.config []
    Inst.Im {} -> Im
    Inst.Sc {} -> Sc

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
    , meta_im_performances :: !(Map BlockId ImPerformance)
    } deriving (Eq, Show, Generics.Generic)

empty_meta :: Meta
empty_meta = Meta
    { meta_creation = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , meta_last_save = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , meta_notes = ""
    , meta_midi_performances = mempty
    , meta_lilypond_performances = mempty
    , meta_im_performances = mempty
    }

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
im_performances = Lens.lens meta_im_performances
    (\f r -> r { meta_im_performances = f (meta_im_performances r) })

type MidiPerformance = Performance (Vector.Vector Midi.WriteMessage)
type LilypondPerformance = Performance Text
type ImPerformance = Performance (Vector.Vector Shared.Note.Note)

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
    perf_events :: !a
    -- | The time this performance was recorded.
    , perf_creation :: !Time.UTCTime
    -- | Free text, containing the git commit when this performance was taken.
    , perf_commit :: !Text
    } deriving (Eq, Show, Functor)

make_performance :: a -> IO (Performance a)
make_performance events = do
    time <- Time.getCurrentTime
    commit <- either (errorIO . txt) return =<< SourceControl.current "."
    return $ Performance
        { perf_events = events
        , perf_creation = time
        , perf_commit = Text.unlines $ map ($ commit)
            [ SourceControl._hash
            , SourceControl.showDate . SourceControl._date
            , SourceControl._summary
            ]
        }

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

empty_default :: Default
empty_default = Default { default_tempo = 1 }

tempo = Lens.lens default_tempo
    (\f r -> r { default_tempo = f (default_tempo r) })

instance Pretty Config where
    format (Config namespace meta root allocations lily dflt saved_views ky
            tscore) =
        Pretty.record "Config"
            [ ("namespace", Pretty.format namespace)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("allocations", Pretty.format allocations)
            , ("lilypond", Pretty.format lily)
            , ("default", Pretty.format dflt)
            , ("saved_views", Pretty.format saved_views)
            , ("ky", Pretty.format ky)
            , ("tscore", Pretty.format tscore)
            ]

instance Pretty Meta where format = Pretty.formatG_
instance Pretty MidiPerformance where
    format = format_performance "MidiPerformance" Vector.length
instance Pretty LilypondPerformance where
    format = format_performance "LilypondPerformance" (Text.count "\n")
instance Pretty ImPerformance where
    format = format_performance "ImPerformance" Vector.length

format_performance :: Pretty b => Pretty.Doc -> (a -> b) -> Performance a
    -> Pretty.Doc
format_performance name format_events (Performance events creation commit) =
    Pretty.record name
        [ ("events", Pretty.format $ format_events events)
        , ("creation", Pretty.text $ pretty creation)
        , ("commit", Pretty.text commit)
        ]

instance Pretty Default where
    format (Default tempo) = Pretty.record "Default"
        [ ("tempo", Pretty.format tempo) ]

instance DeepSeq.NFData Default where
    rnf (Default tempo) = tempo `seq` ()

-- | This is a place to save sets of views so you can switch between them.
-- The ViewId is the one with focus.
type SavedViews = Map Text (Map ViewId Block.View, Maybe ViewId)
