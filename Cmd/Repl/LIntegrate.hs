-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with derive and score integration.
module Cmd.Repl.LIntegrate where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * create

-- | Create an integrated block from the focused block.  The block integrate
-- call will automatically create one block, but you have to use this if you
-- want more than one.  Actually, you can use it on a block without a `<<`
-- integrate call, but there isn't much point since it won't reintegrate until
-- you add one.
block :: Cmd.M m => m ViewId
block = do
    sourceBlock <- Cmd.getFocusedBlock
    rulerId <- Ui.blockRuler sourceBlock
    destBlock <- Create.block rulerId
    Ui.setIntegratedBlock destBlock $
        Just (sourceBlock, Block.DeriveDestinations [])
    Cmd.deriveImmediately [sourceBlock]
    Cmd.inflictBlockDamage sourceBlock
    Create.view destBlock

-- | Create a block integrate copy of the selected block.  Details at
-- 'Block.ScoreDestinations'.
scoreBlock :: Cmd.M m => m ViewId
scoreBlock = do
    sourceBlock <- Cmd.getFocusedBlock
    rulerId <- Ui.blockRuler sourceBlock
    destBlock <- Create.block rulerId
    Ui.setIntegratedBlock destBlock $
        Just (sourceBlock, Block.ScoreDestinations [])
    Cmd.inflictBlockDamage sourceBlock
    Create.view destBlock

-- | Similar to 'block', explicitly create another track integrated from the
-- selected one, which should already have a `<` integrate call on it.
track :: Cmd.M m => m ()
track = do
    (blockId, _, trackId, _) <- Selection.getInsert
    Ui.modifyIntegratedTracks blockId
        ((trackId, Block.DeriveDestinations []) :)
    Cmd.deriveImmediately [blockId]
    Cmd.inflictTrackDamage blockId trackId

-- | Create a track integrate copy of the selected track.  Details at
-- 'Block.ScoreDestinations'.
scoreTrack :: Cmd.M m => m ()
scoreTrack = do
    (blockId, _, trackId, _) <- Selection.getInsert
    Ui.modifyIntegratedTracks blockId
        ((trackId, Block.ScoreDestinations []) :)
    Cmd.inflictTrackDamage blockId trackId

clearScoreTrack :: Cmd.M m => m ()
clearScoreTrack = do
    (blockId, _, trackIds, _) <- Selection.tracks
    Ui.modifyIntegratedTracks blockId $ filter ((`notElem` trackIds) . fst)

clearScoreTracksOf :: Ui.M m => BlockId -> m ()
clearScoreTracksOf blockId =
    Ui.modifyIntegratedTracks blockId (const [])


-- * revert

-- | Revert the selected range back to the integrated state.
selRevert :: Cmd.M m => m ()
selRevert = do
    (blockId, _, trackIds, range) <- Selection.tracks
    Edit.clearRange trackIds range
    byDest <- Block.destinationToSource <$> Ui.getBlock blockId
    sequence_
        [ Ui.insertBlockEvents blockId trackId
            (map Event.unmodified (Map.elems index))
        | (trackId, (_, index)) <- byDest
        , trackId `elem` trackIds
        ]

deleteManual :: Cmd.M m => Block.SourceKey -> m ()
deleteManual key = do
    blockId <- Cmd.getFocusedBlock
    Ui.setIntegratedManual blockId key Nothing


-- * inspect

trackSources :: Cmd.M m => m Text
trackSources = do
    block <- Ui.getBlock =<< Cmd.getFocusedBlock
    pure $ Text.unlines $ List.intercalate [""] $ map showSource $
        Block.blockIntegratedTracks block
    where
    showSource (source, dests) =
        "=== source: " <> pretty source : showTrackDests dests

showTrackDests :: Block.TrackDestinations -> [Text]
showTrackDests = \case
    Block.DeriveDestinations dests -> concatMap showDest dests
    Block.ScoreDestinations dests -> concatMap showScoreDest dests

showScoreDest :: (TrackId, (TrackId, Block.EventIndex)) -> [Text]
showScoreDest (source, (dest, events)) =
    "== " <> pretty source <> " -> " <> pretty dest <> ":"
    : showIndex events

showDest :: Block.NoteDestination -> [Text]
showDest (Block.NoteDestination key note controls) =
    "== key: " <> pretty key
        : "== note: " <> pretty (fst note) : showIndex (snd note)
        ++ concatMap showControl (Map.toList controls)
    where
    showControl (name, (trackId, index)) =
        "== control " <> name <> ": " <> pretty trackId
        : showIndex index

showIndex :: Block.EventIndex -> [Text]
showIndex = map showEvent . Map.elems

showEvent :: Event.Event -> Text
showEvent e = mconcat
    [ pretty (Event.start e), "+", pretty (Event.duration e)
    , " ", pretty (Event.text e)
    , " ", fromMaybe "?" $
        Stack.prettyUiInner . Event.stackStack =<< Event.stack e
    ]

-- | Show the integration state in an abbreviated way.
-- This is an inverse mapping from dest to source.
destToSources :: Cmd.M m => m [(TrackId, (Block.Source, Text))]
destToSources = do
    block <- Ui.getBlock =<< Cmd.getFocusedBlock
    pure $ map (fmap (fmap Block.shortEventIndex)) $
        Block.destinationToSource block

selEdits :: Cmd.M m => m ([Event.IndexKey], [Merge.Edit])
selEdits = do
    (blockId, _, trackId, _) <- Selection.getInsert
    edits blockId trackId

edits :: Cmd.M m => BlockId -> TrackId -> m ([Event.IndexKey], [Merge.Edit])
edits blockId trackId = do
    block <- Ui.getBlock blockId
    index <- Cmd.require "track is not integrated from anywhere" $
        lookup trackId $ indicesOf (Block.blockIntegrated block)
            (Block.blockIntegratedTracks block)
    events <- Ui.getEvents trackId
    let (deleted, edits) = Merge.diffEvents index (Events.ascending events)
    pure (Set.toList deleted, filter Merge.isModified edits)

-- | Show source UI events.
indices :: Cmd.M m => m [(TrackId, (Block.Source, Block.EventIndex))]
indices =
    fmap Block.destinationToSource . Ui.getBlock =<< Cmd.getFocusedBlock

indicesOf :: Maybe (BlockId, Block.TrackDestinations)
    -> [(TrackId, Block.TrackDestinations)] -> [(TrackId, Block.EventIndex)]
indicesOf integrated integratedTracks =
    blockIndices ++ concatMap destIndices integratedTracks
    where
    blockIndices = maybe [] destIndices integrated
    destIndices (_, Block.DeriveDestinations dests) =
        concatMap deriveIndices dests
    destIndices (_, Block.ScoreDestinations dests) = map snd dests
    deriveIndices (Block.NoteDestination _ note controls) =
        note : Map.elems controls

-- | This is always going to be empty because cache strips collect_integrated.
-- That's too bad because sometimes I want to see the original events, for
-- debugging.
integrated :: Cmd.M m => m Text
integrated = do
    integrated <- Cmd.perfIntegrated <$> (Perf.get =<< Cmd.getFocusedBlock)
    pure $ Text.unlines $ concatMap fmt integrated
    where
    fmt (Derive.Integrated source events) =
        pretty source : Stream.shortEvents events
