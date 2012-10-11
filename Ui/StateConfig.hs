{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.StateConfig where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics
import qualified Data.Time as Time

import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Ui.Id as Id
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Types


data Config = Config {
    -- | The default namespace is used for automatically created IDs, so each
    -- project can import other projects without clashes.  The save file is
    -- also derived from the default namespace.
    config_namespace :: !Id.Namespace
    -- | Save into this directory by default.
    , config_project_dir :: !String
    , config_meta :: !Meta
    -- | Derivation can start from any block, but it's useful to know which
    -- block represents the entire piece.  This way, given a position on some
    -- block I can determine where in the piece it lies, if anywhere.  This is
    -- useful for playing a block in proper context, or communicating with
    -- a program with a more absolute notion of time, like a DAW.
    , config_root :: !(Maybe BlockId)

    -- | This maps the midi instruments used in this State to their Addrs.
    , config_midi :: !Instrument.Config
    , config_global_transform :: !String
    , config_default :: !Default
    } deriving (Eq, Read, Show, Generics.Typeable)

namespace = Lens.lens config_namespace (\v r -> r { config_namespace = v })
project_dir =
    Lens.lens config_project_dir (\v r -> r { config_project_dir = v })
meta = Lens.lens config_meta (\v r -> r { config_meta = v })
root = Lens.lens config_root (\v r -> r { config_root = v })
midi = Lens.lens config_midi (\v r -> r { config_midi = v })
global_transform = Lens.lens config_global_transform
    (\v r -> r { config_global_transform = v })
default_ = Lens.lens config_default (\v r -> r { config_default = v })

-- | Extra data that doesn't have any effect on the score.
data Meta = Meta {
    meta_creation :: !Time.UTCTime
    , meta_notes :: !String
    } deriving (Eq, Read, Show, Generics.Typeable)

creation = Lens.lens meta_creation (\v r -> r { meta_creation = v })
notes = Lens.lens meta_notes (\v r -> r { meta_notes = v })

-- | Initial values for derivation.
data Default = Default {
    -- | Automatically created pitch tracks will have this scale.  MIDI thru
    -- will also use it when a scale can't be derived from focus.
    default_scale :: !Pitch.ScaleId
    -- | A key doesn't apply to every scale, but for the ones where it does,
    -- it may be necessary to decide between different enharmonics.
    , default_key :: !(Maybe Pitch.Key)
    -- | This instrument is present in the initial environment, so it will be
    -- the instrument in scope in abscence of any others.
    , default_instrument :: !(Maybe Score.Instrument)
    -- | A toplevel block without a tempo track will get this tempo.
    , default_tempo :: !Signal.Y
    } deriving (Eq, Read, Show, Generics.Typeable)

scale = Lens.lens default_scale (\v r -> r { default_scale = v })
key = Lens.lens default_key (\v r -> r { default_key = v })
instrument = Lens.lens default_instrument (\v r -> r { default_instrument = v })
tempo = Lens.lens default_tempo (\v r -> r { default_tempo = v })

instance Pretty.Pretty Config where
    format (Config namespace dir meta root midi global_transform default_) =
        Pretty.record_title "Config"
            [ ("namespace", Pretty.format namespace)
            , ("project_dir", Pretty.format dir)
            , ("meta", Pretty.format meta)
            , ("root", Pretty.format root)
            , ("midi", Pretty.format midi)
            , ("global_transform", Pretty.format global_transform)
            , ("default", Pretty.format default_)
            ]

instance Pretty.Pretty Meta where
    format (Meta creation notes) = Pretty.record_title "Meta"
        [ ("creation", Pretty.text (show creation))
        , ("notes", Pretty.text notes)
        ]

instance Pretty.Pretty Default where
    format (Default scale key instrument tempo) =
        Pretty.record_title "Default"
            [ ("scale", Pretty.format scale)
            , ("key", Pretty.format key)
            , ("instrument", Pretty.format instrument)
            , ("tempo", Pretty.format tempo)
            ]

instance DeepSeq.NFData Default where
    rnf (Default scale key inst tempo) =
        scale `seq` key `seq` inst `seq` tempo `seq` ()
