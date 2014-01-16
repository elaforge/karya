-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.StateConfig where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Map as Map
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
    } deriving (Eq, Show, Generics.Typeable)

namespace = Lens.lens config_namespace (\v r -> r { config_namespace = v })
meta = Lens.lens config_meta (\v r -> r { config_meta = v })
root = Lens.lens config_root (\v r -> r { config_root = v })
midi = Lens.lens config_midi (\v r -> r { config_midi = v })
global_transform = Lens.lens config_global_transform
    (\v r -> r { config_global_transform = v })
aliases = Lens.lens config_aliases (\v r -> r { config_aliases = v })
lilypond = Lens.lens config_lilypond (\v r -> r { config_lilypond = v })
default_ = Lens.lens config_default (\v r -> r { config_default = v })
saved_views = Lens.lens config_saved_views
    (\v r -> r { config_saved_views = v })

-- | Extra data that doesn't have any effect on the score.
data Meta = Meta {
    meta_creation :: !Time.UTCTime
    , meta_notes :: !Text
    , meta_performances :: !(Map.Map BlockId Performance)
    } deriving (Eq, Read, Show, Generics.Typeable)

creation = Lens.lens meta_creation (\v r -> r { meta_creation = v })
notes = Lens.lens meta_notes (\v r -> r { meta_notes = v })
performances = Lens.lens meta_performances (\v r -> r { meta_performances = v })

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
data Performance = Performance {
    perf_midi :: !(Vector.Vector Midi.WriteMessage)
    -- | The time this performance was recorded.
    , perf_creation :: !Time.UTCTime
    -- | The sequencer's patch level.  For darcs, this should be a patch name
    -- (technically it should be a tag's name, but it doesn't matter as long as
    -- I'm the only developer).  For git, it would be the commit hash.
    , perf_patch :: !Text
    } deriving (Eq, Read, Show, Generics.Typeable)

-- | Initial values for derivation.
--
-- This used to have other fields, but they were replaced by
-- 'config_global_transform'.  I haven't removed tempo yet because it's the
-- only way to change the speed for tempo-less blocks, and doesn't affect
-- (or rather, is undone automatically) for integrated blocks.
data Default = Default {
    -- | A toplevel block without a tempo track will get this tempo.
    default_tempo :: !Signal.Y
    } deriving (Eq, Read, Show, Generics.Typeable)

tempo = Lens.lens default_tempo (\v r -> r { default_tempo = v })

instance Pretty.Pretty Config where
    format (Config namespace meta root midi global_transform aliases lily
            default_ saved_views) =
        Pretty.record_title "Config"
            [ ("namespace", Pretty.format namespace)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("midi", Pretty.format midi)
            , ("global_transform", Pretty.format global_transform)
            , ("aliases", Pretty.format aliases)
            , ("lilypond", Pretty.format lily)
            , ("default", Pretty.format default_)
            , ("saved views", Pretty.format saved_views)
            ]

instance Pretty.Pretty Meta where
    format (Meta creation notes performances) = Pretty.record_title "Meta"
        [ ("creation", Pretty.text (show creation))
        , ("notes", Pretty.text (untxt notes))
        , ("performances", Pretty.format performances)
        ]

instance Pretty.Pretty Performance where
    format (Performance midi creation patch) = Pretty.record_title "Performance"
        [ ("midi", Pretty.text $ show (Vector.length midi))
        , ("creation", Pretty.text $ Pretty.pretty creation)
        , ("patch", Pretty.text (untxt patch))
        ]

instance Pretty.Pretty Default where
    format (Default tempo) =
        Pretty.record_title "Default"
            [ ("tempo", Pretty.format tempo) ]

instance DeepSeq.NFData Default where
    rnf (Default tempo) = tempo `seq` ()

-- | This is a place to save sets of views so you can switch between them.
-- The ViewId is the one with focus.
type SavedViews = Map.Map Text (Map.Map ViewId Block.View, Maybe ViewId)
