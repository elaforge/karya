-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to use manual integration.
module Cmd.Integrate.Manual where
import qualified Data.Map as Map

import qualified App.Config as Config
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge
import qualified Cmd.ModifyNotes as ModifyNotes

import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui

import           Global
import           Types


convertNoteTrack :: Block.SourceKey -> ModifyNotes.NoteTrack
    -> (Convert.Track, [Convert.Track])
convertNoteTrack key (ModifyNotes.NoteTrack notes controls) =
    ( convertTrack ParseTitle.noteTrack notes
    , map convert (Map.toAscList controls)
    )
    where
    convert (ModifyNotes.Pitch scaleId, events) =
        convertTrack (ParseTitle.scaleToTitle scaleId) events
    convert (ModifyNotes.Control control, events) =
        convertTrack (ParseTitle.controlToTitle (ScoreT.untyped control))
            events
    convertTrack title = Convert.Track title
        . map (addStack key) . Events.ascending

addStack :: Block.SourceKey -> Event.Event -> Event.Event
addStack key event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call key) Stack.empty

-- | Create or re-integrate a block with the given tracks.
block :: Ui.M m => Block.SourceKey -> BlockId -> RulerId -> Text
    -> Convert.Tracks -> m (Maybe BlockId)
block sourceKey blockId rulerId blockTitle tracks = do
    (dests, created) <- Ui.lookupBlock blockId >>= \case
        Nothing -> do
            Ui.createBlock (Id.unpackId blockId) blockTitle
                [Block.track (Block.RId rulerId) Config.rulerWidth]
            pure ([], True)
        Just exist -> do
            Ui.setRulerId blockId rulerId
            case Map.lookup sourceKey (Block.blockIntegratedManual exist) of
                Nothing -> Ui.throw $
                    "block to integrate already exists: " <> pretty blockId
                Just dests -> do
                    Ui.setBlockTitle blockId blockTitle
                    pure (dests, False)
    newDests <- Merge.mergeTracks Merge.ReplaceTitles blockId tracks dests
    Ui.setIntegratedManual blockId sourceKey (Just newDests)
    when created $ do
        BlockConfig.toggleMergeAll blockId
    pure $ if created then Just blockId else Nothing
