-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for the Library type.
module Derive.Library (Shadowed, shadowed) where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr

import Global


-- | Warnings for shadowed symbols.  ((call_type, module), symbols)
type Shadowed = ((Text, Module.Module), [Expr.Symbol])

shadowed :: Derive.Library -> [Shadowed]
shadowed (Derive.Library note control pitch val _aliases) =
    filter (not . null . snd) $ concat
        [ call_maps "note" note
        , call_maps "control" control
        , call_maps "pitch" pitch
        , add "val" $ get_shadows Derive.vcall_doc val
        ]
    where
    call_maps tag (Derive.CallMaps gen trans) = concat
        [ add (tag <> " generator") $ get_shadows Derive.call_doc gen
        , add (tag <> " transformer") $ get_shadows Derive.call_doc trans
        ]
    add tag shadows = [((tag, module_), calls) | (module_, calls) <- shadows]

get_shadows :: (call -> Derive.CallDoc) -> [Derive.LookupCall call]
    -> [(Module.Module, [Expr.Symbol])]
get_shadows get_doc = filter (not . null . snd) . map (second duplicates)
    . Seq.group_fst . concatMap (call_module get_doc)

duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe extract . Seq.group_sort id
    where
    extract (x : _ : _) = Just x
    extract _ = Nothing

call_module :: (call -> Derive.CallDoc) -> Derive.LookupCall call
    -> [(Module.Module, Expr.Symbol)]
call_module _ (Derive.LookupPattern {}) = []
call_module get_doc (Derive.LookupMap calls) =
    [ (Derive.cdoc_module (get_doc call), sym)
    | (sym, call) <- Map.toList calls
    ]
