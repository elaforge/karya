-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.Util where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Derive.ScoreT as ScoreT
import qualified Ui.Id as Id

import           Global


type Instrument = Text

-- | Create a 'ScoreT.Instrument'.  Drop a leading @>@, since I often
-- accidentally include one.
instrument :: Instrument -> ScoreT.Instrument
instrument = ScoreT.Instrument . Text.dropWhile (=='>')

matchMap :: Id.Ident id => Text -> Map id a -> Map id a
matchMap match = Map.filterWithKey (\k _ -> matchId match k)

-- | True if the ID contains the given substring.
matchId :: Id.Ident id => Text -> id -> Bool
matchId sub = (sub `Text.isInfixOf`) . Id.showId . Id.unpackId
