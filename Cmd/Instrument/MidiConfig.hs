-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to create MIDI configs.
module Cmd.Instrument.MidiConfig where
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Ui.State as State
import qualified Derive.Env as Env
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import qualified Instrument.InstTypes as InstTypes
import Global


data Config = Config {
    config_midi :: Patch.Configs
    , config_aliases :: Map.Map Score.Instrument InstTypes.Qualified
    } deriving (Show)

instance Pretty.Pretty Config where
    format (Config midi allocations) = Pretty.record "Config"
        [ ("midi", Pretty.format midi)
        , ("allocations", Pretty.format allocations)
        ]

type Instrument = Text
type Alias = Text

merge :: State.M m => Config -> m ()
merge (Config midi allocations) = State.modify $
    (State.config#State.midi %= (midi<>))
    . (State.config#State.allocations %= (allocations<>))

replace :: State.M m => Config -> m ()
replace (Config midi allocations) = State.modify $
    (State.config#State.midi #= midi)
    . (State.config#State.allocations #= allocations)

config :: [(Alias, Instrument, Patch.Config)] -> Config
config configs = Config
    { config_midi =
        Map.fromList [(inst alias, config) | (alias, _, config) <- configs]
    , config_aliases = Map.fromList
        [ (inst alias, InstTypes.parse_qualified name)
        | (alias, name, _) <- configs
        ]
    }
    where inst = Score.Instrument

configs :: [(Text, Patch.Config)] -> Patch.Configs
configs = Map.fromList . map (first Score.Instrument)

environ :: RestrictedEnviron.ToVal a => Env.Key -> a
    -> Patch.Config -> Patch.Config
environ name val = Patch.cenviron %= (RestrictedEnviron.make [(name, v)] <>)
    where v = RestrictedEnviron.to_val val
