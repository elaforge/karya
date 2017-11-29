-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Utilities for the Library type.
module Derive.Library (
    Shadowed, shadowed
    -- * make
    , library, generators, transformers, vals, lookup
    , Calls(..), both
    , poly_generators, poly_transformers
) where
import Prelude hiding (lookup)
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr

import Global


-- | Warnings for shadowed symbols.  ((call_type, module), symbols)
type Shadowed = ((Text, Module.Module), [Expr.Symbol])

shadowed :: Derive.Library -> [Shadowed]
shadowed (Derive.Scopes gen trans track val) =
    filter (not . null . snd) $ concat
        [ call_maps "generator"
            Derive.call_doc Derive.call_doc Derive.call_doc gen
        , call_maps "transformer"
            Derive.call_doc Derive.call_doc Derive.call_doc trans
        , call_maps "track"
            Derive.tcall_doc Derive.tcall_doc Derive.tcall_doc track
        , add "val" $ get_shadows Derive.vcall_doc val
        ]
    where
    call_maps ctype doc1 doc2 doc3 (Derive.Scope note control pitch) = concat
        [ add ("note " <> ctype) $ get_shadows doc1 note
        , add ("control " <> ctype) $ get_shadows doc2 control
        , add ("pitch " <> ctype) $ get_shadows doc3 pitch
        ]
    add ctype shadows =
        [((ctype, module_), calls) | (module_, calls) <- shadows]

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

-- * make

library :: ToLibrary call => [(Expr.Symbol, call)] -> Derive.Library
library [] = mempty
library calls = to_library (Derive.call_map calls) mempty

-- | This is just a specialization of 'library', just for documentation.
generators :: ToLibrary (Derive.Generator call) =>
    [(Expr.Symbol, Derive.Generator call)] -> Derive.Library
generators = library

-- | This is just a specialization of 'library', just for documentation.
transformers :: ToLibrary (Derive.Transformer call) =>
    [(Expr.Symbol, Derive.Transformer call)] -> Derive.Library
transformers = library

-- | This is just a specialization of 'library', just for documentation.
vals :: [(Expr.Symbol, Derive.ValCall)] -> Derive.Library
vals = library

lookup :: ToLibrary call => Derive.LookupCall call -> Derive.Library
lookup c = to_library [c] mempty

-- | Bundle a generator and transformer together, so I can define them
-- together.  Functions to create these are in "Derive.Call.Make".
data Calls d = Calls {
    generator :: !(Derive.Generator d)
    , transformer :: !(Derive.Transformer d)
    }

both :: (ToLibrary (Derive.Generator d), ToLibrary (Derive.Transformer d)) =>
    [(Expr.Symbol, Calls d)] -> Derive.Library
both sym_calls =
    generators (zip syms (map generator calls))
        <> transformers (zip syms (map transformer calls))
    where (syms, calls) = unzip sym_calls

-- | Add a polymorphic generator to all call types.
--
-- The Callable constraint is not needed here, but callers will have it, and
-- for some reason you can't coerce a parametric variable into a constrained
-- one.
poly_generators ::
    (forall d. Derive.Callable d => [(Expr.Symbol, Derive.Generator d)])
    -> Derive.Library
poly_generators calls = mconcat
    [ generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Note)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Control)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)])
    ]

poly_transformers ::
    (forall d. Derive.Callable d => [(Expr.Symbol, Derive.Transformer d)])
    -> Derive.Library
poly_transformers calls = mconcat
    [ transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Note)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Control)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Pitch)])
    ]

-- ** ToLibrary

class ToLibrary call where
    to_library :: [Derive.LookupCall call] -> Derive.Library -> Derive.Library

instance ToLibrary (Derive.Generator Derive.Note) where
    to_library = (Derive.s_generator#Derive.s_note #=)
instance ToLibrary (Derive.Generator Derive.Control) where
    to_library = (Derive.s_generator#Derive.s_control #=)
instance ToLibrary (Derive.Generator Derive.Pitch) where
    to_library = (Derive.s_generator#Derive.s_pitch #=)

instance ToLibrary (Derive.Transformer Derive.Note) where
    to_library = (Derive.s_transformer#Derive.s_note #=)
instance ToLibrary (Derive.Transformer Derive.Control) where
    to_library = (Derive.s_transformer#Derive.s_control #=)
instance ToLibrary (Derive.Transformer Derive.Pitch) where
    to_library = (Derive.s_transformer#Derive.s_pitch #=)

instance ToLibrary Derive.ValCall where
    to_library = (Derive.s_val #=)
