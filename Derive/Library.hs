-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
-- | Utilities for the Library type.
module Derive.Library (
    -- * make
    Library, library, generators, transformers, vals, pattern
    , ToLibrary, Entry(..)
    , Calls(..), both
    , poly_generators, poly_transformers
    -- * compile
    , Shadowed, compile, compile_log
) where
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Map
import qualified Util.Seq as Seq

import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr

import Global


-- | The holds the libary of statically-declared calls.  It gets compiled to
-- 'Derive.Builtins' by 'compile'.
type Library = Derive.ScopesT
    (MkScope Derive.Generator)
    (MkScope Derive.Transformer)
    (MkScope Derive.TrackCall)
    [Entry Derive.ValCall]

type MkScope kind =
    Derive.Scope [Entry (kind Derive.Note)] [Entry (kind Derive.Control)]
        [Entry (kind Derive.Pitch)]

data Entry call = Single !Expr.Symbol !call | Pattern !(Derive.PatternCall call)

instance Show Library where show _ = "((Library))"
instance Pretty (Entry call) where
    pretty (Single sym _) = Expr.unsym sym
    pretty (Pattern pattern) = "pattern:" <> Derive.pat_description pattern

-- * make

library :: ToLibrary call => [(Expr.Symbol, call)] -> Library
library [] = mempty
library calls = to_library (map (uncurry Single) calls) mempty

-- | This is just a specialization of 'library', just for documentation.
generators :: ToLibrary (Derive.Generator call) =>
    [(Expr.Symbol, Derive.Generator call)] -> Library
generators = library

-- | This is just a specialization of 'library', just for documentation.
transformers :: ToLibrary (Derive.Transformer call) =>
    [(Expr.Symbol, Derive.Transformer call)] -> Library
transformers = library

-- | This is just a specialization of 'library', just for documentation.
vals :: [(Expr.Symbol, Derive.ValCall)] -> Library
vals = library

pattern :: ToLibrary call => Derive.PatternCall call -> Library
pattern c = to_library [Pattern c] mempty

-- | Bundle a generator and transformer together, so I can define them
-- together.  Functions to create these are in "Derive.Call.Make".
data Calls d = Calls {
    generator :: !(Derive.Generator d)
    , transformer :: !(Derive.Transformer d)
    }

both :: (ToLibrary (Derive.Generator d), ToLibrary (Derive.Transformer d)) =>
    [(Expr.Symbol, Calls d)] -> Library
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
    -> Library
poly_generators calls = mconcat
    [ generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Note)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Control)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)])
    ]

poly_transformers ::
    (forall d. Derive.Callable d => [(Expr.Symbol, Derive.Transformer d)])
    -> Library
poly_transformers calls = mconcat
    [ transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Note)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Control)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Pitch)])
    ]

-- ** ToLibrary

class ToLibrary call where
    to_library :: [Entry call] -> Library -> Library

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

-- * compile

-- | Warnings for shadowed symbols.  ((call_type, module), symbols)
type Shadowed = ((Text, Module.Module), [Expr.Symbol])

-- | Convert Library to Builtins.  This indexes by module and also gives me
-- a place to emit warnings about duplicate symbol names.
compile :: Library -> (Derive.Builtins, [Shadowed])
compile (Derive.Scopes lgen ltrans ltrack lval) = Logger.runId $ Derive.Scopes
    <$> compile_scope Derive.call_doc Derive.call_doc Derive.call_doc lgen
    <*> compile_scope Derive.call_doc Derive.call_doc Derive.call_doc ltrans
    <*> compile_scope Derive.tcall_doc Derive.tcall_doc Derive.tcall_doc ltrack
    <*> compile_entries "val" Derive.vcall_doc lval
    where
    compile_scope doc1 doc2 doc3 (Derive.Scope note control pitch) =
        Derive.Scope
            <$> compile_entries "note" doc1 note
            <*> compile_entries "control" doc2 control
            <*> compile_entries "pitch" doc3 pitch
    compile_entries kind get_doc = fmap Map.fromAscList
        . traverse (compile1 kind)
        . Seq.keyed_group_sort (Derive.cdoc_module . entry_doc)
        where
        entry_doc (Single _ call) = get_doc call
        entry_doc (Pattern pattern) = Derive.pat_call_doc pattern
    compile1 kind (module_, entries) = do
        let (singles, patterns) = partition entries
        let (cmap, dups) = Util.Map.unique singles
        unless (null dups) $
            Logger.log ((kind, module_), map fst dups)
        return $ (module_,) $ Derive.CallMap
            { call_map = cmap
            , call_patterns = patterns
            }
    partition = Either.partitionEithers . map partition1
    partition1 (Single sym call) = Left (sym, call)
    partition1 (Pattern pattern) = Right pattern

compile_log :: Log.LogMonad m => Library -> m Derive.Builtins
compile_log lib = do
    let (builtins, shadows) = compile lib
    forM_ shadows $ \((call_type, _module), calls) ->
        Log.warn $ call_type <> " shadowed: " <> pretty calls
    return builtins
