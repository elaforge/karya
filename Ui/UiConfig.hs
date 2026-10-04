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
    , emptyConfig
    , namespace_, meta, root, allocations, lilypond, default_, savedViews
    , ky, tscore
    , allocationsMap
    , verifyAllocations
    , Allocations(..), unallocations
    , makeAllocations
    , midiAllocations
    , modifyAllocation
    , Allocation(..)
    , allocation
    , hasIm, hasMidi, hasSc
    , isImAllocation, isMidiAllocation
    , playCache

    , Backend(..), backendName
    , midiConfig
    , convertBackend
    , Meta(..)
    , emptyMeta
    , creation, lastSave, midiPerformances, lilypondPerformances
    , imPerformances
    , MidiPerformance, LilypondPerformance, ImPerformance
    , Performance(..)
    , makePerformance
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
    { configNamespace :: Id.Namespace
    , configMeta :: Meta
    -- | Derivation can start from any block, but it's useful to know which
    -- block represents the entire piece.  This way, given a position on some
    -- block I can determine where in the piece it lies, if anywhere.  This is
    -- useful for playing a block in proper context, or communicating with
    -- a program with a more absolute notion of time, like a DAW.
    , configRoot :: Maybe BlockId

    , configAllocations :: Allocations
    , configLilypond :: Lilypond.Config
    , configDefault :: Default
    , configSavedViews :: SavedViews
    -- | Locally defined code in the ky language, as parsed by
    -- 'Derive.Parse.parse_ky'.  If the ky defines a note transformer called
    -- @GLOBAL@, it will be implicitly wrapped around every derivation.
    , configKy :: Text
    , configTscore :: Text
    } deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config
    { configNamespace = Id.namespace "untitled"
    , configMeta = emptyMeta
    , configRoot = Nothing
    , configAllocations = mempty
    , configLilypond = Lilypond.defaultConfig
    , configDefault = emptyDefault
    , configSavedViews = mempty
    , configKy = ""
    , configTscore = ""
    }

-- Ui.State already has a function called 'namespace'.
namespace_ = Lens.lens configNamespace (\r a -> r { configNamespace = a })
meta = Lens.lens configMeta (\r a -> r { configMeta = a })
root = Lens.lens configRoot (\r a -> r { configRoot = a })
allocations = Lens.lens configAllocations
    (\r a -> r { configAllocations = a })
lilypond = Lens.lens configLilypond (\r a -> r { configLilypond = a })
default_ = Lens.lens configDefault (\r a -> r { configDefault = a })
savedViews = Lens.lens configSavedViews
    (\r a -> r { configSavedViews = a })
ky = Lens.lens configKy (\r a -> r { configKy = a })
tscore = Lens.lens configTscore (\r a -> r { configTscore = a })

-- | Unwrap the newtype for convenience.
allocationsMap :: Lens Config (Map ScoreT.Instrument Allocation)
allocationsMap = Lens.lens (open . configAllocations)
    (\r a -> r { configAllocations = Allocations a })
    where open (Allocations a) = a

-- | TODO Make this the Allocations constructor?  Probably not worth it.
verifyAllocations :: (InstT.Qualified -> Maybe Inst.Backend) -> Allocations
    -> [Text]
verifyAllocations lookupBackend allocs =
    verifyBackendsMatch lookupBackend allocs
    ++ verifyNoOverlappingMidi allocs

verifyBackendsMatch :: (InstT.Qualified -> Maybe Inst.Backend) -> Allocations
    -> [Text]
verifyBackendsMatch lookupBackend (Allocations allocs) =
    concatMap check (Map.toList allocs)
    where
    check (inst, alloc) = maybe [] ((:[]) . ((pretty inst <> ": ")<>)) $
        case lookupBackend allocQualified of
            Nothing -> Just $
                "patch not found: " <> pretty allocQualified
            Just backend -> case (allocBackend, backend) of
                (Midi {}, Inst.Midi {}) -> Nothing
                (Im, Inst.Im {}) -> Nothing
                (Sc, Inst.Sc {}) -> Nothing
                (Dummy {}, Inst.Dummy {}) -> Nothing
                _ -> Just $ "allocation type " <> backendName allocBackend
                    <> " /= instrument type " <> Inst.backendName backend
        where Allocation { allocQualified, allocBackend } = alloc

verifyNoOverlappingMidi :: Allocations -> [Text]
verifyNoOverlappingMidi (Allocations allocs) =
    concatMap check $ Lists.zipNexts instAddrs
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
    instAddrs =
        [ (inst, byDevice config)
        | (inst, alloc) <- Map.toList allocs
        , Midi config <- [allocBackend alloc]
        ]
    byDevice = Map.fromList . Lists.groupFst . map fst
        . Patch.configAllocation

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
makeAllocations :: [(ScoreT.Instrument, Allocation)] -> Allocations
makeAllocations = Allocations . Map.fromList

-- | This is 'make_allocations' specialized for MIDI instruments.  Like
-- 'make_allocations', it also does no verification.
midiAllocations :: [(ScoreT.Instrument, (InstT.Qualified, Patch.Config))]
    -> Allocations
midiAllocations allocs = Allocations $ Map.fromList
    [ (inst, allocation qual (Midi config))
    | (inst, (qual, config)) <- allocs
    ]

modifyAllocation :: ScoreT.Instrument -> (Allocation -> Either Text Allocation)
    -> Allocations -> Either Text Allocations
modifyAllocation instrument modify (Allocations allocs) = do
    alloc <- justErr ("no allocation for " <> pretty instrument) $
        Map.lookup instrument allocs
    newAlloc <- modify alloc
    unless (sameBackend (allocBackend alloc) (allocBackend newAlloc)) $
        Left "modify_allocation changed the backend"
    pure $ Allocations $ Map.insert instrument newAlloc allocs

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
    { allocQualified :: InstT.Qualified
    , allocConfig :: Common.Config
    , allocBackend :: Backend
    } deriving (Eq, Show)

allocation :: InstT.Qualified -> Backend -> Allocation
allocation qualified backend = Allocation
    { allocQualified = qualified
    , allocConfig = Common.emptyConfig
    , allocBackend = backend
    }

instance Pretty Allocation where
    format (Allocation qualified config backend) = Pretty.record "Allocation"
        [ ("qualified", Pretty.format qualified)
        , ("config", Pretty.format config)
        , ("backend", Pretty.format backend)
        ]

hasIm :: Allocations -> Bool
hasIm = any isImAllocation . Map.elems . unallocations

hasMidi :: Allocations -> Bool
hasMidi = any isMidiAllocation
    . filter ((/= playCache) . allocQualified) . Map.elems . unallocations

hasSc :: Allocations -> Bool
hasSc = any isScAllocation . Map.elems . unallocations

playCache :: InstT.Qualified
playCache = InstT.Qualified "play-cache" ""

isImAllocation :: Allocation -> Bool
isImAllocation alloc = case allocBackend alloc of
    Im -> True
    _ -> False

isMidiAllocation :: Allocation -> Bool
isMidiAllocation alloc = case allocBackend alloc of
    Midi {} -> True
    _ -> False

isScAllocation :: Allocation -> Bool
isScAllocation alloc = case allocBackend alloc of
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
backendName :: Backend -> Text
backendName = \case
    Midi {} -> "midi"
    Im -> "im"
    Sc -> "sc"
    Dummy {} -> "dummy"

sameBackend :: Backend -> Backend -> Bool
sameBackend b1 b2 = case (b1, b2) of
    (Midi {}, Midi {}) -> True
    (Im, Im) -> True
    (Sc, Sc) -> True
    (Dummy {}, Dummy {}) -> True
    _ -> False

midiConfig :: Backend -> Maybe Patch.Config
midiConfig (Midi config) = Just config
midiConfig _ = Nothing

convertBackend :: Inst.Backend -> Backend
convertBackend = \case
    Inst.Dummy {} -> Dummy ""
    Inst.Midi {} -> Midi $ Patch.config []
    Inst.Im {} -> Im
    Inst.Sc {} -> Sc

-- | Extra data that doesn't have any effect on the score.
data Meta = Meta
    -- | The time the score was created.  This should be reset whenever
    -- the score is started, or copied from a template.
    { metaCreation :: Time.UTCTime
    -- | The last time the score was saved.  This is useful to determine which
    -- of several saves is the latest.
    , metaLastSave :: Time.UTCTime
    , metaMidiPerformances :: Map BlockId MidiPerformance
    , metaLilypondPerformances :: Map BlockId LilypondPerformance
    , metaImPerformances :: Map BlockId ImPerformance
    } deriving (Eq, Show, Generics.Generic)

emptyMeta :: Meta
emptyMeta = Meta
    { metaCreation = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , metaLastSave = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    , metaMidiPerformances = mempty
    , metaLilypondPerformances = mempty
    , metaImPerformances = mempty
    }

creation = Lens.lens metaCreation (\r a -> r { metaCreation = a })
lastSave = Lens.lens metaLastSave (\r a -> r { metaLastSave = a })
midiPerformances = Lens.lens metaMidiPerformances
    (\r a -> r { metaMidiPerformances = a })
lilypondPerformances = Lens.lens metaLilypondPerformances
    (\r a -> r { metaLilypondPerformances = a })
imPerformances = Lens.lens metaImPerformances
    (\r a -> r { metaImPerformances = a })

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
data Performance a = Performance
    { perfEvents :: a
    -- | The time this performance was recorded.
    , perfCreation :: Time.UTCTime
    -- | Free text, containing the git commit when this performance was taken.
    , perfCommit :: Text
    } deriving (Eq, Show, Functor)

makePerformance :: a -> IO (Performance a)
makePerformance events = do
    time <- Time.getCurrentTime
    commit <- either (errorIO . txt) pure =<< SourceControl.current "."
    pure $ Performance
        { perfEvents = events
        , perfCreation = time
        , perfCommit = Text.unlines $ map ($ commit)
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
newtype Default = Default
    -- | A toplevel block without a tempo track will get this tempo.
    { defaultTempo :: Signal.Y
    } deriving (Eq, Read, Show)

emptyDefault :: Default
emptyDefault = Default { defaultTempo = 1 }

tempo = Lens.lens defaultTempo (\r a -> r { defaultTempo = a })

instance Pretty Config where
    format (Config namespace meta root allocations lily dflt savedViews ky
            tscore) =
        Pretty.record "Config"
            [ ("namespace", Pretty.format namespace)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("allocations", Pretty.format allocations)
            , ("lilypond", Pretty.format lily)
            , ("default", Pretty.format dflt)
            , ("saved_views", Pretty.format savedViews)
            , ("ky", Pretty.format ky)
            , ("tscore", Pretty.format tscore)
            ]

instance Pretty Meta where format = Pretty.formatGCamel
instance Pretty MidiPerformance where
    format = formatPerformance "MidiPerformance" Vector.length
instance Pretty LilypondPerformance where
    format = formatPerformance "LilypondPerformance" (Text.count "\n")
instance Pretty ImPerformance where
    format = formatPerformance "ImPerformance" Vector.length

formatPerformance :: Pretty b => Pretty.Doc -> (a -> b) -> Performance a
    -> Pretty.Doc
formatPerformance name formatEvents (Performance events creation commit) =
    Pretty.record name
        [ ("events", Pretty.format $ formatEvents events)
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
