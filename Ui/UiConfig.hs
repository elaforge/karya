-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
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
    , verify_allocations
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified GHC.Generics as Generics

import qualified Util.Lens as Lens
import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
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
data Config = Config
    -- | The default namespace is used for automatically created IDs, so each
    -- project can import other projects without clashes.
    { config_namespace :: Id.Namespace
    , config_meta :: Meta
    -- | Derivation can start from any block, but it's useful to know which
    -- block represents the entire piece.  This way, given a position on some
    -- block I can determine where in the piece it lies, if anywhere.  This is
    -- useful for playing a block in proper context, or communicating with
    -- a program with a more absolute notion of time, like a DAW.
    , config_root :: Maybe BlockId

    , config_allocations :: Allocations
    , config_lilypond :: Lilypond.Config
    , config_default :: Default
    , config_saved_views :: SavedViews
    -- | Locally defined code in the ky language, as parsed by
    -- 'Derive.Parse.parse_ky'.  If the ky defines a note transformer called
    -- @GLOBAL@, it will be implicitly wrapped around every derivation.
    , config_ky :: Text
    , config_tscore :: Text
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
namespace_ = Lens.lens config_namespace (\r a -> r { config_namespace = a })
meta = Lens.lens config_meta (\r a -> r { config_meta = a })
root = Lens.lens config_root (\r a -> r { config_root = a })
allocations = Lens.lens config_allocations
    (\r a -> r { config_allocations = a })
lilypond = Lens.lens config_lilypond (\r a -> r { config_lilypond = a })
default_ = Lens.lens config_default (\r a -> r { config_default = a })
saved_views = Lens.lens config_saved_views
    (\r a -> r { config_saved_views = a })
ky = Lens.lens config_ky (\r a -> r { config_ky = a })
tscore = Lens.lens config_tscore (\r a -> r { config_tscore = a })

-- | Unwrap the newtype for convenience.
allocations_map :: Lens Config (Map ScoreT.Instrument Allocation)
allocations_map = Lens.lens (open . config_allocations)
    (\r a -> r { config_allocations = Allocations a })
    where open (Allocations a) = a

-- | TODO Make this the Allocations constructor?  Probably not worth it.
verify_allocations :: (InstT.Qualified -> Maybe Inst.Backend) -> Allocations
    -> [Text]
verify_allocations lookup_backend allocs =
    verify_backends_match lookup_backend allocs
    ++ verify_no_overlapping_midi allocs

verify_backends_match :: (InstT.Qualified -> Maybe Inst.Backend) -> Allocations
    -> [Text]
verify_backends_match lookup_backend (Allocations allocs) =
    concatMap check (Map.toList allocs)
    where
    check (inst, alloc) = maybe [] ((:[]) . ((pretty inst <> ": ")<>)) $
        case lookup_backend alloc_qualified of
            Nothing -> Just $
                "patch not found: " <> pretty alloc_qualified
            Just backend -> case (alloc_backend, backend) of
                (Midi {}, Inst.Midi {}) -> Nothing
                (Im, Inst.Im {}) -> Nothing
                (Sc, Inst.Sc {}) -> Nothing
                (Dummy {}, Inst.Dummy {}) -> Nothing
                _ -> Just $ "allocation type " <> backend_name alloc_backend
                    <> " /= instrument type " <> Inst.backend_name backend
        where Allocation { alloc_qualified, alloc_backend } = alloc

verify_no_overlapping_midi :: Allocations -> [Text]
verify_no_overlapping_midi (Allocations allocs) =
    concatMap check $ Lists.zipNexts inst_addrs
    where
    check (inst1, rest) = concatMap (overlaps inst1) rest
    overlaps (inst1, devs1) (inst2, devs2) = do
        (dev, Lists.Both chans1 chans2) <- Maps.pairs devs1 devs2
        let overlap = Set.intersection
                (Set.fromList chans1) (Set.fromList chans2)
        guard (overlap /= mempty)
        pure $ Text.unwords $
            [ pretty inst1, "and", pretty inst2, "on", pretty dev
            , "overlap chans:"
            ] ++ map (showt . (+1)) (Set.toList overlap)
    inst_addrs =
        [ (inst, by_device config)
        | (inst, alloc) <- Map.toList allocs
        , Midi config <- [alloc_backend alloc]
        ]
    by_device = Map.fromList . Lists.groupFst . map fst
        . Patch.config_allocation

-- TODO I'm not a big fan of this name, since it's generic and not
-- obviously related to instruments.  However the previous name,
-- 'aliases', was too and I somehow lived through that.  I tried
-- 'instruments', but it seemed too easy to confuse with
-- 'ScoreT.Instrument'.  But in ky, it's in the instruments: section.
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
data Allocation = Allocation
    { alloc_qualified :: InstT.Qualified
    , alloc_config :: Common.Config
    , alloc_backend :: Backend
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
data Backend
    = Midi Patch.Config
    | Im
    | Sc
    -- | This is for instruments without a backend.  For example a paired
    -- instrument might be written as one instrument, but realized as two
    -- different ones.  It should be resolved to concrete instruments during
    -- derivation, and includes an error msg show if that doesn't happen.
    -- If it's "", inherit the msg from its 'Inst.Dummy', if there is one.
    | Dummy Text
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
    Im -> "im"
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
    meta_creation :: Time.UTCTime
    -- | The last time the score was saved.  This is useful to determine which
    -- of several saves is the latest.
    , meta_last_save :: Time.UTCTime
    , meta_notes :: Text
    , meta_midi_performances :: Map BlockId MidiPerformance
    , meta_lilypond_performances :: Map BlockId LilypondPerformance
    , meta_im_performances :: Map BlockId ImPerformance
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

creation = Lens.lens meta_creation (\r a -> r { meta_creation = a })
last_save = Lens.lens meta_last_save (\r a -> r { meta_last_save = a })
notes = Lens.lens meta_notes (\r a -> r { meta_notes = a })
midi_performances = Lens.lens meta_midi_performances
    (\r a -> r { meta_midi_performances = a })
lilypond_performances = Lens.lens meta_lilypond_performances
    (\r a -> r { meta_lilypond_performances = a })
im_performances = Lens.lens meta_im_performances
    (\r a -> r { meta_im_performances = a })

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
    perf_events :: a
    -- | The time this performance was recorded.
    , perf_creation :: Time.UTCTime
    -- | Free text, containing the git commit when this performance was taken.
    , perf_commit :: Text
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
newtype Default = Default {
    -- | A toplevel block without a tempo track will get this tempo.
    default_tempo :: Signal.Y
    } deriving (Eq, Read, Show)

empty_default :: Default
empty_default = Default { default_tempo = 1 }

tempo = Lens.lens default_tempo (\r a -> r { default_tempo = a })

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
