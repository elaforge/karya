module Cmd.Repl.Util where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Ui.Id as Id


match_map :: (Id.Ident id) => String -> Map.Map id a -> Map.Map id a
match_map match = Map.filterWithKey (\k _ -> match_id match k)

-- | True if the ID contains the given substring.
match_id :: (Id.Ident id) => String -> id -> Bool
match_id sub = (sub `List.isInfixOf`) . Id.show_id . Id.unpack_id
