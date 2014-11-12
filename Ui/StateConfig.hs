-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.StateConfig where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Perform.Lilypond.Types as Lilypond
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Signal as Signal

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

    -- | This maps the midi instruments used in this State to their Configs.
    , config_midi :: !Instrument.Configs
    -- | This is a tracklang transformer expression that's wrapped around every
    -- derivation.  So if it's @x = 1 | y = 2@ then the environment will be so
    -- modified for every derivation.
    --
    -- It's useful to set config as above, or to apply a postproc.  Of course
    -- these can also be done within a block, but if you put it on the root
    -- block then playing in sub-blocks doesn't get the transformation, and
    -- if you put it in sub-blocks then you repeat yourself and possibly apply
    -- it multiple times.
    , config_global_transform :: !Text
    -- | Local instrument aliases.  Map instruments through this map before
    -- setting them, so this goes from instrument alias to underlying
    -- instrument.
    , config_aliases :: !(Map.Map Score.Instrument Score.Instrument)
    , config_lilypond :: !Lilypond.Config
    , config_default :: !Default
    , config_saved_views :: !SavedViews
    -- | If set, load local definitions from this file.  The filename is
    -- relative to the score directory, which is defined by the loading code.
    , config_ky_file :: !(Maybe FilePath)
    } deriving (Eq, Show)

-- Ui.State already has a function called 'namespace'.
namespace_ = Lens.lens config_namespace
    (\f r -> r { config_namespace = f (config_namespace r) })
meta = Lens.lens config_meta
    (\f r -> r { config_meta = f (config_meta r) })
root = Lens.lens config_root
    (\f r -> r { config_root = f (config_root r) })
midi = Lens.lens config_midi
    (\f r -> r { config_midi = f (config_midi r) })
global_transform = Lens.lens config_global_transform
    (\f r -> r { config_global_transform = f (config_global_transform r) })
aliases = Lens.lens config_aliases
    (\f r -> r { config_aliases = f (config_aliases r) })
lilypond = Lens.lens config_lilypond
    (\f r -> r { config_lilypond = f (config_lilypond r) })
default_ = Lens.lens config_default
    (\f r -> r { config_default = f (config_default r) })
saved_views = Lens.lens config_saved_views
    (\f r -> r { config_saved_views = f (config_saved_views r) })
ky_file = Lens.lens config_ky_file
    (\f r -> r { config_ky_file = f (config_ky_file r) })

empty_config :: Config
empty_config = Config
    { config_namespace = Id.namespace "untitled"
    , config_meta = empty_meta
    , config_root = Nothing
    , config_midi = Instrument.configs []
    , config_global_transform = ""
    , config_aliases = mempty
    , config_lilypond = Lilypond.default_config
    , config_default = empty_default
    , config_saved_views = mempty
    , config_ky_file = Nothing
    }

-- | Extra data that doesn't have any effect on the score.
data Meta = Meta {
    -- | The time the score was created.  This should be reset whenever
    -- the score is started, or copied from a template.
    meta_creation :: !Time.UTCTime
    -- | The last time the score was saved.  This is useful to determine which
    -- of several saves is the latest.
    , meta_last_save :: !Time.UTCTime
    , meta_notes :: !Text
    , meta_midi_performances :: !(Map.Map BlockId MidiPerformance)
    , meta_lilypond_performances :: !(Map.Map BlockId LilypondPerformance)
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
-- This used to have other fields, but they were replaced by
-- 'config_global_transform'.  I haven't removed tempo yet because it's the
-- only way to change the speed for tempo-less blocks, and doesn't affect
-- (or rather, is undone automatically) for integrated blocks.
data Default = Default {
    -- | A toplevel block without a tempo track will get this tempo.
    default_tempo :: !Signal.Y
    } deriving (Eq, Read, Show)

tempo = Lens.lens default_tempo
    (\f r -> r { default_tempo = f (default_tempo r) })

empty_default :: Default
empty_default = Default { default_tempo = 1 }

instance Pretty.Pretty Config where
    format (Config namespace meta root midi global_transform aliases lily
            default_ saved_views definition) =
        Pretty.record "Config"
            [ ("namespace", Pretty.format namespace)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("midi", Pretty.format midi)
            , ("global_transform", Pretty.format global_transform)
            , ("aliases", Pretty.format aliases)
            , ("lilypond", Pretty.format lily)
            , ("default", Pretty.format default_)
            , ("saved views", Pretty.format saved_views)
            , ("definition file", Pretty.format definition)
            ]

instance Pretty.Pretty Meta where
    format (Meta creation last_save notes midi_perf lily_perf) =
        Pretty.record "Meta"
        [ ("creation", Pretty.format creation)
        , ("last save", Pretty.format last_save)
        , ("notes", Pretty.text (untxt notes))
        , ("midi performances", Pretty.format midi_perf)
        , ("lilypond performances", Pretty.format lily_perf)
        ]

instance Pretty.Pretty MidiPerformance where
    format (Performance midi creation patch) = Pretty.record "MidiPerformance"
        [ ("events", Pretty.format $ Vector.length midi)
        , ("creation", Pretty.text $ pretty creation)
        , ("patch", Pretty.text (untxt patch))
        ]

instance Pretty.Pretty LilypondPerformance where
    format (Performance ly creation patch) = Pretty.record "LilypondPerformance"
        [ ("lilypond lines", Pretty.format $ Text.count "\n" ly)
        , ("creation", Pretty.text $ pretty creation)
        , ("patch", Pretty.text (untxt patch))
        ]

instance Pretty.Pretty Default where
    format (Default tempo) = Pretty.record "Default"
        [ ("tempo", Pretty.format tempo) ]

instance DeepSeq.NFData Default where
    rnf (Default tempo) = tempo `seq` ()

-- | This is a place to save sets of views so you can switch between them.
-- The ViewId is the one with focus.
type SavedViews = Map.Map Text (Map.Map ViewId Block.View, Maybe ViewId)
