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

import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.InstTypes as InstTypes
import Global


data Config = Config {
    config_midi :: Instrument.Configs
    , config_aliases :: Map.Map Score.Instrument InstTypes.Qualified
    } deriving (Show)

instance Pretty.Pretty Config where
    format (Config midi aliases) = Pretty.record "Config"
        [ ("midi", Pretty.format midi)
        , ("aliases", Pretty.format aliases)
        ]

type Instrument = Text
type Alias = Text

merge :: State.M m => Config -> m ()
merge (Config midi aliases) = State.modify $
    (State.config#State.midi %= (midi<>))
    . (State.config#State.aliases %= (aliases<>))

replace :: State.M m => Config -> m ()
replace (Config midi aliases) = State.modify $
    (State.config#State.midi #= midi) . (State.config#State.aliases #= aliases)

config :: [(Alias, Instrument, Instrument.Config)] -> Config
config configs = Config
    { config_midi =
        Map.fromList [(inst alias, config) | (alias, _, config) <- configs]
    , config_aliases = Map.fromList
        [ (inst alias, InstTypes.parse_qualified name)
        | (alias, name, _) <- configs
        ]
    }
    where inst = Score.Instrument

configs :: [(Text, Instrument.Config)] -> Instrument.Configs
configs = Map.fromList . map (first Score.Instrument)

environ :: RestrictedEnviron.ToVal a => Env.Key -> a
    -> Instrument.Config -> Instrument.Config
environ name val =
    Instrument.cenviron %= (RestrictedEnviron.make [(name, v)] <>)
    where v = RestrictedEnviron.to_val val
