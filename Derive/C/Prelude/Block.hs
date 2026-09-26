-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Block call and support.
module Derive.C.Prelude.Block (
    library
    , evalRootBlock, globalTransform
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Derive.Args as Args
import qualified Derive.Cache as Cache
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [(BlockUtil.captureNullControl, cCaptureNullControl)]
    , Library.pattern patternNoteBlock
    , Library.pattern patternControlBlock
    ]

-- * root block

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
--
-- Tempo.with_tempo does a bit of magic to stretch all blocks to length 1,
-- except the root one.  The root block should operate in real time, so no
-- stretching here.  Otherwise, a tempo of 2 would be the same as 1.
{-# SCC evalRootBlock #-}
evalRootBlock :: BlockId -> Derive.NoteDeriver
evalRootBlock blockId =
    globalTransform $ Eval.evalOneCall True $ callFromBlockId blockId

{-# SCC globalTransform #-}
globalTransform :: Derive.NoteDeriver -> Derive.NoteDeriver
globalTransform = transformIfPresent ctx "GLOBAL"
    where ctx = Derive.dummyContext 0 1 "<GLOBAL transform>"

transformIfPresent :: Derive.Callable (Derive.Transformer a)
    => Derive.Context a
    -> Expr.Symbol -> Derive.Deriver (Stream.Stream a)
    -> Derive.Deriver (Stream.Stream a)
transformIfPresent ctx sym deriver = Derive.lookupCall sym >>= \case
    Nothing -> deriver
    Just call -> Eval.applyTransformer ctx call [] deriver

-- * note calls

patternNoteBlock :: Derive.PatternCall (Derive.Generator Derive.Note)
patternNoteBlock = Derive.PatternCall
    { patDescription = "block name"
    , patDoc = Derive.extractDoc fakeCall
    , patFunction = \sym -> fmap cBlock <$> callToBlockId sym
    }
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fakeCall = cBlock (Id.BlockId (Id.readId "example/block"))

{-# SCC cBlock #-}
cBlock :: BlockId -> Derive.Generator Derive.Note
cBlock blockId = Derive.withScoreDuration getScoreDuration $
    Derive.withRealDuration (const $ getRealDuration blockId) $
    Derive.generator Module.prelude
        (Derive.CallName $ "block " <> showt blockId) Tags.prioBlock
    "Substitute the named block into the score. If the symbol doesn't contain\
    \ a `/`, the default namespace is applied. If it starts with a `-`, this\
    \ is a relative call and the calling block's namespace and name are\
    \ prepended."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        stack <- Internal.getStack
        when (Stack.Block blockId `elem` Stack.innermost stack) $
            Derive.throw $ "recursive call to " <> pretty blockId
        -- This ensures that each block call from the same event sees a new
        -- serial number, as documented in 'Derive.CacheKey'.
        Internal.incrementEventSerial
        -- I have to put the block on the stack before calling 'd_block'
        -- because 'Cache.block' relies on on the block id already being
        -- on the stack.
        Internal.withStackBlock blockId $ Cache.block blockId run args
    where
    run args = Derive.place start (end - start) $ trim args $ dBlock blockId
        where (start, end) = Args.range args
    trim args deriver = do
        end <- Derive.real (1 :: ScoreTime)
        if Event.isPositive (Args.event args)
            then trimControls end deriver
            else deriver
    getScoreDuration :: a -> Derive.Deriver (Derive.CallDuration ScoreTime)
    getScoreDuration _ = Derive.CallDuration . (\(s, e) -> e-s) <$>
        Derive.blockLogicalRange blockId
    getRealDuration :: BlockId
        -> Derive.Deriver (Derive.CallDuration RealTime)
    getRealDuration blockId = fmap (either (const Derive.Unknown) id) $
        Derive.getRealDuration $ Internal.withStackBlock blockId $
            dBlock blockId

-- | Remove samples at the given RealTime.  This is to support final block
-- notes and 'Derive.Flags.infer_duration'.  Otherwise, an event at the end of
-- a block will pick up controls (e.g. a transposition change) that is meant to
-- start at the beginning of the next block.
--
-- However, I can't trim the signal entirely because I still want the control
-- signals past the end of the block to be visible to non-final events.  The
-- problem is that I do want control samples at the end time to be visible
-- if they are interior to the block.  So I need a way to tell the difference
-- between controls from the caller and local ones, so a final event can
-- exclude the former and include the latter.
--
-- TODO this is unsatisfying because it feels ad-hoc and clunky.  In addition,
-- it drops a sample that I don't actually want to drop for non-final events.
-- Not to mention it's inefficient.
trimControls :: RealTime -> Derive.Deriver a -> Derive.Deriver a
trimControls end =
    Derive.modifySignals (Signal.dropDiscontinuityAt end)
        (PSignal.dropDiscontinuityAt end)

dBlock :: BlockId -> Derive.NoteDeriver
dBlock blockId = do
    blocks <- Derive.getUiState Ui.stateBlocks
    -- Do some error checking.  These are all caught later, but if I throw here
    -- I can give more specific error msgs.
    title <- case Map.lookup blockId blocks of
        Nothing -> Derive.throw "block_id not found"
        Just block -> pure $ Block.blockTitle block
    -- Record a dependency on this block.  This should happen even if the
    -- derivation throws, because a dependency on a block with an error is
    -- still a dependency.
    Internal.addBlockDep blockId
    let deriver = BlockUtil.noteDeriver blockId
    fmap (fromMaybe Stream.empty) $ Derive.catch True $
        case ParseTitle.parseBlock title of
            Left err -> Derive.throw $ "block title: " <> err
            -- An empty title means no transformation.
            Right (Expr.Call call [] :| []) | call == "" -> deriver
            Right expr ->
                Eval.evalTransformers ctx (NonEmpty.toList expr) deriver
                where ctx = Derive.dummyContext 0 1 "block title"

-- | Given a block id, produce a call expression that will call that block.
callFromBlockId :: BlockId -> DeriveT.Call
callFromBlockId blockId =
    Expr.call0 (Expr.Symbol $ Id.showId $ Id.unpackId blockId)

-- | Like 'Eval.call_to_block_id' but make sure the block exists.
callToBlockId :: Expr.Symbol -> Derive.Deriver (Maybe BlockId)
callToBlockId sym = do
    caller <- Internal.lookupCurrentBlockId
    ns <- Derive.getUiState $ UiConfig.configNamespace . Ui.stateConfig
    case Eval.callToBlockId ns caller sym of
        Nothing -> pure Nothing
        Just blockId -> do
            blocks <- Derive.getUiState Ui.stateBlocks
            pure $ if Map.member blockId blocks then Just blockId
                else Nothing

-- * control calls

patternControlBlock :: Derive.PatternCall (Derive.Generator Derive.Control)
patternControlBlock = Derive.PatternCall
    { patDescription = "block name"
    , patDoc = Derive.extractDoc fakeCall
    , patFunction = \sym -> fmap cControlBlock <$> callToBlockId sym
    }
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fakeCall = cControlBlock (Id.BlockId (Id.readId "example/block"))

cControlBlock :: BlockId -> Derive.Generator Derive.Control
cControlBlock blockId = Derive.generator Module.prelude "control-block"
    mempty ("Substitute the control signal from the named control block.\
    \ A control block should consist of a single branch ending in\
    \ a track named `%`.  The signal from that track will be\
    \ substituted."
    ) $
    Sig.call0 $ \args -> do
        let (start, end) = Args.range args
        Derive.place start (end-start) (dControlBlock blockId)

dControlBlock :: BlockId -> Derive.ControlDeriver
dControlBlock blockId = Internal.withStackBlock blockId $ do
    -- Control calls aren't cached, so I can put the block stack in the
    -- convenient place.
    blocks <- Derive.getUiState Ui.stateBlocks
    when (Map.lookup blockId blocks == Nothing) $
        Derive.throw "block_id not found"
    Internal.addBlockDep blockId
    deriver <- Derive.evalUi $ BlockUtil.controlDeriver blockId
    deriver

cCaptureNullControl :: Derive.Generator Derive.Note
cCaptureNullControl = Derive.generator1 Module.internal
    (Derive.symToCallName BlockUtil.captureNullControl) mempty
    ("This is an internal call used to capture the control signal at the\
    \ bottom of a control block."
    ) $ Sig.call0 $ \_ -> do
        sig <- Derive.require "no null control to capture"
            =<< Derive.lookupSignal Controls.null
        stack <- Derive.getStack
        pure $! Score.setControl Controls.null sig Score.emptyEvent
            { Score.eventStack = stack }
