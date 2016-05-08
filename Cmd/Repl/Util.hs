-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.Util where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Ui.Id as Id
import qualified Derive.Score as Score
import Global


type Instrument = Text

-- | Create a 'Score.Instrument'.  Drop a leading @>@, since I often
-- accidentally include one.
instrument :: Instrument -> Score.Instrument
instrument = Score.Instrument . Text.dropWhile (=='>')

match_map :: Id.Ident id => Text -> Map.Map id a -> Map.Map id a
match_map match = Map.filterWithKey (\k _ -> match_id match k)

-- | True if the ID contains the given substring.
match_id :: Id.Ident id => Text -> id -> Bool
match_id sub = (sub `Text.isInfixOf`) . Id.show_id . Id.unpack_id
