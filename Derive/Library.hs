-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Utilities for the Library type.
module Derive.Library (
    -- * make
    Library, library, generators, transformers, vals, pattern
    , ToLibrary, Entry(..)
    , Calls(..), both
    , polyGenerators, polyTransformers
    -- * compile
    , Shadowed, showShadowed, compile, compileLog
) where
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Maps as Maps

import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Expr as Expr

import           Global


-- | The holds the libary of statically-declared calls.  It gets compiled to
-- 'Derive.Builtins' by 'compile'.
type Library = Derive.ScopesT
    (Derive.Scope
        [Entry (Derive.Generator Derive.Note)]
        [Entry (Derive.Generator Derive.Control)]
        [Entry (Derive.Generator Derive.Pitch)])
    (Derive.Scope
        [Entry (Derive.Transformer Derive.Note)]
        [Entry (Derive.Transformer Derive.Control)]
        [Entry (Derive.Transformer Derive.Pitch)])
    (Derive.Scope
        [Entry (Derive.TrackCall Derive.Note)]
        [Entry (Derive.TrackCall Derive.Control)]
        [Entry (Derive.TrackCall Derive.Pitch)])
    [Entry Derive.ValCall]

data Entry call
    = Single !Expr.Symbol !call
    | Pattern !(Derive.PatternCall call)

instance Show Library where show _ = "((Library))"
instance Pretty (Entry call) where
    pretty (Single sym _) = Expr.unsym sym
    pretty (Pattern pattern) = "pattern:" <> Derive.patDescription pattern

-- * make

library :: ToLibrary call => [(Expr.Symbol, call)] -> Library
library [] = mempty
library calls = toLibrary (map (uncurry Single) calls) mempty

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
pattern c = toLibrary [Pattern c] mempty

-- | Bundle a generator and transformer together, so I can define them
-- together.  Functions to create these are in "Derive.Call.Make".
data Calls d = Calls
    { generator :: !(Derive.Generator d)
    , transformer :: !(Derive.Transformer d)
    }

both :: (ToLibrary (Derive.Generator d), ToLibrary (Derive.Transformer d)) =>
    [(Expr.Symbol, Calls d)] -> Library
both symCalls =
    generators (zip syms (map generator calls))
        <> transformers (zip syms (map transformer calls))
    where (syms, calls) = unzip symCalls

-- | Add a polymorphic generator to all call types.
--
-- The Callable constraint is not needed here, but callers will have it, and
-- for some reason you can't coerce a parametric variable into a constrained
-- one.
polyGenerators ::
    (forall d. Derive.CallableExpr d => [(Expr.Symbol, Derive.Generator d)])
    -> Library
polyGenerators calls = mconcat
    [ generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Note)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Control)])
    , generators (calls :: [(Expr.Symbol, Derive.Generator Derive.Pitch)])
    ]

polyTransformers ::
    (forall d. Derive.CallableExpr d => [(Expr.Symbol, Derive.Transformer d)])
    -> Library
polyTransformers calls = mconcat
    [ transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Note)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Control)])
    , transformers (calls :: [(Expr.Symbol, Derive.Transformer Derive.Pitch)])
    ]

-- ** ToLibrary

class ToLibrary call where
    toLibrary :: [Entry call] -> Library -> Library

instance ToLibrary (Derive.Generator Derive.Note) where
    toLibrary = (Derive.sGenerator#Derive.sNote #=)
instance ToLibrary (Derive.Generator Derive.Control) where
    toLibrary = (Derive.sGenerator#Derive.sControl #=)
instance ToLibrary (Derive.Generator Derive.Pitch) where
    toLibrary = (Derive.sGenerator#Derive.sPitch #=)

instance ToLibrary (Derive.Transformer Derive.Note) where
    toLibrary = (Derive.sTransformer#Derive.sNote #=)
instance ToLibrary (Derive.Transformer Derive.Control) where
    toLibrary = (Derive.sTransformer#Derive.sControl #=)
instance ToLibrary (Derive.Transformer Derive.Pitch) where
    toLibrary = (Derive.sTransformer#Derive.sPitch #=)

instance ToLibrary (Derive.TrackCall Derive.Note) where
    toLibrary = (Derive.sTrack#Derive.sNote #=)
instance ToLibrary (Derive.TrackCall Derive.Control) where
    toLibrary = (Derive.sTrack#Derive.sControl #=)
instance ToLibrary (Derive.TrackCall Derive.Pitch) where
    toLibrary = (Derive.sTrack#Derive.sPitch #=)

instance ToLibrary Derive.ValCall where
    toLibrary = (Derive.sVal #=)

-- * compile

-- | Warnings for shadowed symbols.  ((call_type, module), symbols)
type Shadowed = ((Text, Module.Module), [(Expr.Symbol, [Derive.CallName])])
-- type Shadowed = ((Text, Module.Module), [Expr.Symbol])

showShadowed :: Shadowed -> Text
showShadowed ((name, Module.Module module_), calls) = Text.unwords $ mconcat
    [ ["shadowed", name, "calls in module", module_ <> ":"]
    , [ Expr.unsym sym <> " bound by ["
        <> Text.unwords (map unname names) <> "]"
      | (sym, names) <- calls
      ]
    ]
    where unname (Derive.CallName n) = n

-- | Convert Library to Builtins.  This indexes by module and also gives me
-- a place to emit warnings about duplicate symbol names.
compile :: Library -> (Derive.Builtins, [Shadowed])
compile (Derive.Scopes lgen ltrans ltrack lval) = Logger.runId $ Derive.Scopes
    <$> compileScope lgen
    <*> compileScope ltrans
    <*> compileScope ltrack
    <*> compileEntries "val" lval
    where
    compileScope (Derive.Scope note control pitch) = Derive.Scope
        <$> compileEntries "note" note
        <*> compileEntries "control" control
        <*> compileEntries "pitch" pitch
    compileEntries kind =
        fmap Map.fromAscList
        . traverse (compile1 kind)
        . Lists.keyedGroupSort (Derive.cdocModule . entryDoc)
        where
        entryDoc (Single _ call) = Derive.callDoc call
        entryDoc (Pattern pattern) = Derive.patCallDoc pattern
    compile1 kind (module_, entries) = do
        let (singles, patterns) = partition entries
        let (cmap, dups) = Maps.unique2 singles
        unless (null dups) $ Logger.log
            ( (kind, module_)
            , map (second (map Derive.callName)) dups
            )
        pure $ (module_,) $ Derive.CallMap
            { callMap = cmap
            , callPatterns = patterns
            }
    partition = Either.partitionEithers . map partition1
    partition1 (Single sym call) = Left (sym, call)
    partition1 (Pattern pattern) = Right pattern

compileLog :: Log.LogMonad m => Library -> m Derive.Builtins
compileLog lib = do
    let (builtins, shadows) = compile lib
    forM_ shadows $ \((callType, _module), calls) ->
        Log.warn $ callType <> " shadowed: " <> pretty calls
    pure builtins
