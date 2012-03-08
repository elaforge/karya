{-# LANGUAGE DeriveDataTypeable #-}
-- | State.Config and State.Default, in their own module to avoid circular
-- imports with "State.Update".  Everyone else should pretend they're defined
-- in "Ui.State".
module Ui.StateConfig where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Generics as Generics

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
    -- | Derivation can start from any block, but it's useful to know which
    -- block represents the entire piece.  This way, given a position on some
    -- block I can determine where in the piece it lies, if anywhere.  This is
    -- useful for playing a block in proper context, or communicating with
    -- a program with a more absolute notion of time, like a DAW.
    , config_root :: !(Maybe BlockId)

    -- | This maps the midi instruments used in this State to their Addrs.
    , config_midi :: !Instrument.Config
    , config_default :: !Default
    } deriving (Eq, Read, Show, Generics.Typeable)

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

instance DeepSeq.NFData Default where
    rnf (Default scale key inst tempo) =
        scale `seq` key `seq` inst `seq` tempo `seq` ()
