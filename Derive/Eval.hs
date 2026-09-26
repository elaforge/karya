-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Evaluate tracklang expressions.
module Derive.Eval (
    -- * eval / apply
    evalToplevel, evalQuoted, evalQuotedNormalized, evalExprVal
    -- ** generator
    , applyGenerator
    -- ** transformer
    , evalTransformers, evalTransformExpr
    , applyTransformer, applyTransformers
    , evalQuotedTransformers
    -- ** val calls
    , eval, apply
    , getValCall

    -- * lookup call
    , getGenerator, getTransformer, getTrackCall
    , unknownSymbol, callToBlockId, blockIdToCall
    -- ** relative calls
    , isRelative, makeRelative, parseRelativeId

    -- * util
    , evalOne, evalOneCall, evalOneAt
    , evalEvent, reapplyGenerator, reapplyGeneratorNormalized
    , reapply, reapplyCall, evalPitch, evalNote, applyPitch
    , evalExpr
) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * eval / apply

-- | Apply a toplevel expression.
{-# SCC evalToplevel #-}
evalToplevel :: Derive.CallableExpr d => Derive.Context d
    -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
evalToplevel ctx expr =
    evalTransformers ctx transformCalls (evalGenerator ctx generatorCall)
    where (transformCalls, generatorCall) = Expr.split expr

evalQuoted :: Derive.CallableExpr d => Derive.Context d -> DeriveT.Quoted
    -> Derive.Deriver (Stream.Stream d)
evalQuoted ctx (DeriveT.Quoted expr) = evalToplevel ctx expr

-- | This is like 'eval_quoted', except that the 'Derive.ctx_event' is set to
-- (0, 1) normalized time.  This is important if you want to place the
-- resulting deriver.  Otherwise, you can use eval_quoted and the event's
-- position will fall through to the callee.
--
-- TODO this awkwardness is because events evaluate in track time, not in
-- normalized time.  Details in "Derive.EvalTrack".
evalQuotedNormalized :: Derive.CallableExpr d => Derive.Context d
    -> DeriveT.Quoted -> Derive.Deriver (Stream.Stream d)
evalQuotedNormalized = evalQuoted . normalizeEvent

normalizeEvent :: Derive.Context val -> Derive.Context val
normalizeEvent ctx = ctx
    { Derive.ctxEvent = Event.place 0 1 (Derive.ctxEvent ctx)
    , Derive.ctxPrevEvents = []
    , Derive.ctxNextEvents = []
    }

-- | Eval a Expr MiniVal, which is produced by 'Expr.ToExpr'.
evalExprVal :: Derive.CallableExpr d => Derive.Context d
    -> Expr.Expr Expr.MiniVal -> Derive.Deriver (Stream.Stream d)
evalExprVal ctx expr = evalToplevel ctx (convertMinival expr)

-- TODO find a better place for this, or get rid of MiniVal
convertMinival :: Expr.Expr Expr.MiniVal -> DeriveT.Expr
convertMinival = fmap $ fmap $ \case
    Expr.VNum v -> DeriveT.VSignal $ Signal.constant <$> v
    Expr.VStr v -> DeriveT.VStr v

-- ** generator

{-# SCC evalGenerator #-}
evalGenerator :: (Derive.Callable (Derive.Generator d), Derive.Taggable d)
    => Derive.Context d -> DeriveT.Call -> Derive.Deriver (Stream.Stream d)
evalGenerator ctx (Expr.Call sym args) = do
    vals <- mapM (eval ctx) args
    call <- getGenerator sym
    applyGenerator ctx call vals

-- | Like 'eval_generator', but for when the args are already parsed and
-- evaluated.  This is useful when one generator wants to dispatch to another.
{-# SCC applyGenerator #-}
applyGenerator :: Derive.Context d -> Derive.Generator d -> [DeriveT.Val]
    -> Derive.Deriver (Stream.Stream d)
applyGenerator ctx call args = do
    let passed = Derive.PassedArgs
            { Derive.passedVals = args
            , Derive.passedCallName = Derive.callName call
            , Derive.passedCtx = ctx
            }
    mode <- Derive.getMode
    Internal.withStackCall (Derive.callName call) $ case mode of
        Derive.ScoreDurationQuery -> do
            dur <- Derive.gfuncScoreDuration (Derive.callFunc call) passed
            setScoreDuration dur
            pure Stream.empty
        Derive.RealDurationQuery -> do
            dur <- Derive.gfuncRealDuration (Derive.callFunc call) passed
            setRealDuration dur
            pure Stream.empty
        _ -> do
            -- Ensure a unique serial number for each generator call, as
            -- documneted in 'Stack.Serial'.
            serial <- Derive.gets $
                Derive.stateEventSerial . Derive.stateThreaded
            Internal.withStackSerial serial $
                Derive.gfuncF (Derive.callFunc call) passed

-- | See 'Derive.CallDuration' for details.
setScoreDuration :: Derive.CallDuration ScoreTime -> Derive.Deriver ()
setScoreDuration dur = Internal.modifyCollect $ \collect ->
    collect { Derive.collectScoreDuration = dur }

setRealDuration :: Derive.CallDuration RealTime -> Derive.Deriver ()
setRealDuration dur = Internal.modifyCollect $ \collect ->
    collect { Derive.collectRealDuration = dur }

-- ** transformer

{-# SCC evalTransformers #-}
evalTransformers :: (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Derive.Context d -> [DeriveT.Call] -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
evalTransformers ctx calls deriver = go calls
    where
    go [] = deriver
    go (Expr.Call sym args : calls) = do
        vals <- mapM (eval ctx) args
        call <- getTransformer sym
        applyTransformer ctx call vals (go calls)

-- | Parse and apply a transformer expression.
evalTransformExpr ::
    (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Text -> Text -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
evalTransformExpr name exprStr deriver
    | Text.all Char.isSpace exprStr = deriver
    | otherwise = do
        expr <- case Parse.parseExpr exprStr of
            Left err -> Derive.throw $ name <> ": " <> err
            Right expr -> pure expr
        let ctx = Derive.dummyContext 0 1 name
        evalTransformers ctx (NonEmpty.toList expr) deriver

-- | The same as 'eval_transformers', but get them out of a Quoted.
evalQuotedTransformers ::
    (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Derive.Context d -> DeriveT.Quoted -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
evalQuotedTransformers ctx (DeriveT.Quoted expr) =
    evalTransformers ctx (NonEmpty.toList expr)

-- | The transformer version of 'apply_generator'.  Like 'eval_transformers',
-- but apply only one, and apply to already evaluated 'DeriveT.Val's.  This
-- is useful when you want to re-apply an already parsed set of vals.
{-# SCC applyTransformer #-}
applyTransformer :: Derive.Context d -> Derive.Transformer d -> [DeriveT.Val]
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
applyTransformer ctx call args deriver =
    Internal.withStackCall (Derive.callName call) $
        Derive.callFunc call passed deriver
    where
    passed = Derive.PassedArgs
        { passedVals = args
        , passedCallName = Derive.callName call
        , passedCtx = ctx
        }

-- | A list version of 'apply_transformer'.
applyTransformers :: Derive.Context d
    -> [(Derive.Transformer d, [DeriveT.Val])]
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
applyTransformers ctx calls deriver = foldr apply deriver calls
    where apply (sym, args) = applyTransformer ctx sym args

-- ** val call

{-# SCC eval #-}
eval :: Derive.Taggable a => Derive.Context a -> DeriveT.Term
    -> Derive.Deriver DeriveT.Val
eval _ (Expr.Literal val) = pure val
eval ctx (Expr.ValCall (Expr.Call sym terms)) = do
    call <- getValCall sym
    apply (Derive.tagContext ctx) call terms

{-# SCC apply #-}
apply :: Derive.Context Derive.Tagged -> Derive.ValCall
    -> [DeriveT.Term] -> Derive.Deriver DeriveT.Val
apply ctx call args = do
    vals <- mapM (eval ctx) args
    let passed = Derive.PassedArgs
            { passedVals = vals
            , passedCallName = Derive.callName call
            , passedCtx = ctx
            }
    Internal.withStackCall (Derive.callName call) $
        Derive.callFunc call passed

-- * lookup call

getValCall :: Expr.Symbol -> Derive.Deriver Derive.ValCall
getValCall sym = requireCall False sym name =<< Derive.lookupCall sym
    where name = Derive.callableName (Proxy :: Proxy Derive.ValCall)

getGenerator :: forall d. Derive.Callable (Derive.Generator d) =>
    Expr.Symbol -> Derive.Deriver (Derive.Generator d)
getGenerator sym = requireCall True sym name =<< Derive.lookupCall sym
    where name = Derive.callableName (Proxy :: Proxy (Derive.Generator d))

getTransformer :: forall d. Derive.Callable (Derive.Transformer d) =>
    Expr.Symbol -> Derive.Deriver (Derive.Transformer d)
getTransformer sym =
    requireCall False sym name =<< Derive.lookupCall sym
    where name = Derive.callableName (Proxy :: Proxy (Derive.Transformer d))

getTrackCall :: forall d. Derive.Callable (Derive.TrackCall d) =>
    Expr.Symbol -> Derive.Deriver (Derive.TrackCall d)
getTrackCall sym =
    requireCall False sym name =<< Derive.lookupCall sym
    where name = Derive.callableName (Proxy :: Proxy (Derive.TrackCall d))

requireCall :: Bool -> Expr.Symbol -> Text -> Maybe a -> Derive.Deriver a
requireCall _ _ _ (Just a) = pure a
requireCall isGenerator sym name Nothing = do
    -- If the call wasn't found, it can be seen as a block call whose block
    -- doesn't exist yet.  If it is created later, I have to know that this
    -- block depends on it, otherwise it won't be rederived and hence won't
    -- realize that the bad call is now valid.
    when isGenerator $ do
        caller <- Internal.lookupCurrentBlockId
        ns <- Derive.getUiState $ UiConfig.configNamespace . Ui.stateConfig
        whenJust (callToBlockId ns caller sym) Internal.addBlockDep
    Derive.throw $ unknownSymbol name sym

unknownSymbol :: Text -> Expr.Symbol -> Text
unknownSymbol name sym = name <> " not found: " <> ShowVal.showVal sym

-- | Given a Symbol, try to come up with the BlockId of the block it could be
-- a call for.
callToBlockId :: Id.Namespace -> Maybe BlockId
    -- ^ If the symbol starts with -, this block is prepended to it.
    -> Expr.Symbol -> Maybe BlockId
callToBlockId ns maybeCaller sym
    | sym == "" = Nothing
    | otherwise = Just $ Id.BlockId $ Id.readShort ns relative
    where
    relative
        | Just caller <- maybeCaller, isRelative sym =
            Id.identText caller <> Expr.unsym sym
        | otherwise = Expr.unsym sym

-- | Create the symbol to call a given block.
blockIdToCall :: Bool -> BlockId -> BlockId -> Text
blockIdToCall relative parent child
    | Id.identNamespace parent /= Id.identNamespace child =
        Id.showId (Id.unpackId child)
    | relative, Just suffix <- Text.stripPrefix
            (parentName <> relativeSeparator) childName =
        relativeSeparator <> suffix
    | otherwise = childName
    where
    childName = Id.identName child
    parentName = Id.identName parent

-- | True if this is a relative block call.
isRelative :: Expr.Symbol -> Bool
isRelative = (relativeSeparator `Text.isPrefixOf`) . Expr.unsym

-- | Make a block name relative to a parent block.
makeRelative :: BlockId -> Text -> Text
makeRelative blockId name =
    Id.identName blockId <> relativeSeparator <> name

-- | If it's a relative BlockId, return the parent and relative call.
parseRelativeId :: BlockId -> Maybe (BlockId, Id.Id)
parseRelativeId blockId
    | Text.null post = Nothing
    | otherwise = Just
        ( Id.BlockId $ Id.setName pre (Id.unpackId blockId)
        , Id.id (Id.identNamespace blockId) (Text.drop 1 post)
        )
    where (pre, post) = Text.breakOn relativeSeparator $ Id.identName blockId

relativeSeparator :: Text
relativeSeparator = "-"

-- * util

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
evalOne :: Derive.CallableExpr d => Bool -> DeriveT.Expr
    -> Derive.Deriver (Stream.Stream d)
evalOne collect = evalOneAt collect 0 1

evalOneCall :: Derive.CallableExpr d => Bool -> DeriveT.Call
    -> Derive.Deriver (Stream.Stream d)
evalOneCall collect = evalOne collect . (:| [])

evalOneAt :: Derive.CallableExpr d => Bool -> ScoreTime -> ScoreTime
    -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
evalOneAt collect start dur expr = evalExpr collect ctx expr
    where
    -- Set the event start and duration instead of using Derive.place since
    -- this way I can have zero duration events.
    ctx = Derive.dummyContext start dur $ ShowVal.showVal expr

-- | Like 'Derive.EvalTrack.derive_event' but evaluate the event outside of its
-- track context.  This is useful if you want to evaluate things out of order,
-- i.e. evaluate the /next/ pitch.
evalEvent :: Derive.CallableExpr d => Event.Event
    -> Derive.Deriver (Either Text (Stream.Stream d))
evalEvent event = case Parse.parseExpr (Event.text event) of
    Left err -> pure $ Left err
    Right expr -> Right <$>
        -- TODO eval it separately to catch any exception?
        evalOneAt False (Event.start event) (Event.duration event) expr

-- | Evaluate a generator, reusing the passed args but replacing the Symbol.
-- Generators can use this to delegate to other generators.
reapplyGenerator :: Derive.Callable (Derive.Generator d)
    => Derive.PassedArgs d -> Expr.Symbol -> Derive.Deriver (Stream.Stream d)
reapplyGenerator args sym = do
    let ctx = Derive.passedCtx args
    call <- getGenerator sym
    applyGenerator ctx call (Derive.passedVals args)

-- | Like 'reapply_generator', but the note is given normalized time, 0--1,
-- instead of inheriting the start and duration from the args.  This is
-- essential if you want to shift or stretch the note.
reapplyGeneratorNormalized :: Derive.Callable (Derive.Generator d)
    => Derive.PassedArgs d -> Expr.Symbol -> Derive.Deriver (Stream.Stream d)
reapplyGeneratorNormalized args = reapplyGenerator $ args
    { Derive.passedCtx = ctx
        { Derive.ctxEvent = Event.place 0 1 (Derive.ctxEvent ctx)
        , Derive.ctxEventEnd = 1
        }
    }
    where ctx = Derive.passedCtx args

-- | Apply an expr with an explicit Context.  You can use this to reuse the
-- current call's Context, but be careful because it will also inherit the
-- 'Derive.ctx_sub_tracks', which means if inversion hasn't happened yet, which
-- may be what you want or may be surprising.  For instance, it will likely
-- override any pitch you try to set.
reapply :: Derive.CallableExpr d => Derive.Context d -> DeriveT.Expr
    -> Derive.Deriver (Stream.Stream d)
reapply = evalExpr True

reapplyCall :: Derive.CallableExpr d => Derive.Context d -> Expr.Symbol
    -> [DeriveT.Term] -> Derive.Deriver (Stream.Stream d)
reapplyCall ctx sym callArgs =
    reapply ctx (Expr.generator $ Expr.Call sym callArgs)

-- | A version of 'eval' specialized to evaluate pitch calls.  It's unknown if
-- this pitch has been transposed or not.
evalPitch :: ScoreTime -> DeriveT.PitchCall
    -> Derive.Deriver (PSignal.RawPitch a)
evalPitch pos call = do
    pitch <- Typecheck.typecheck ("eval pitch " <> ShowVal.showVal call) pos
        =<< eval ctx (Expr.ValCall call)
    pure $ PSignal.coerce (pitch :: PSignal.Pitch)
    where
    ctx :: Derive.Context Derive.Pitch
    ctx = Derive.dummyContext pos 0 "<eval_pitch>"

-- | Get a Pitch from in a given scale.  Like 'eval_pitch', it's unknown if
-- this pitch has been transposed or not.
evalNote :: Derive.Scale -> Pitch.Note -> Derive.Deriver (PSignal.RawPitch a)
evalNote scale note = case Derive.scaleNoteToCall scale note of
    Nothing -> Derive.throw $ pretty scale <> " has no note " <> pretty note
    Just vcall -> applyPitch 0 vcall

-- | This is like 'eval_pitch' when you already know the call, presumably
-- because you asked 'Derive.scale_note_to_call'.
applyPitch :: ScoreTime -> Derive.ValCall
    -> Derive.Deriver (PSignal.RawPitch a)
applyPitch pos call = do
    pitch <- Typecheck.typecheck msg pos =<< apply ctx call []
    pure $ PSignal.coerce (pitch :: PSignal.Pitch)
    where
    msg = "apply pitch: " <> showt (Derive.callName call)
    ctx = Derive.dummyContext pos 0 "<apply_pitch>"

-- | Evaluate a single expression, catching an exception if it throws.
evalExpr :: Derive.CallableExpr d => Bool -- ^ See 'Derive.catch'.  This
    -- should be True for evals that generate notes for eventual output.
    -> Derive.Context d -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
evalExpr collect ctx expr =
    fromMaybe Stream.empty <$> Derive.catch collect (evalToplevel ctx expr)
