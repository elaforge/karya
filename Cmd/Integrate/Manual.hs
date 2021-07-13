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


convert_note_track :: Block.SourceKey -> ModifyNotes.NoteTrack
    -> (Convert.Track, [Convert.Track])
convert_note_track key (ModifyNotes.NoteTrack notes controls) =
    ( convert_track ParseTitle.note_track notes
    , map convert (Map.toAscList controls)
    )
    where
    convert (ModifyNotes.Pitch scale_id, events) =
        convert_track (ParseTitle.scale_to_title scale_id) events
    convert (ModifyNotes.Control control, events) =
        convert_track (ParseTitle.control_to_title (ScoreT.untyped control))
            events
    convert_track title = Convert.Track title
        . map (add_stack key) . Events.ascending

add_stack :: Block.SourceKey -> Event.Event -> Event.Event
add_stack key event =
    Event.stack_ #= Just (Event.Stack stack (Event.start event)) $ event
    where stack = Stack.add (Stack.Call key) Stack.empty

-- | Create or re-integrate a block with the given tracks.
block :: Ui.M m => Block.SourceKey -> BlockId -> RulerId -> Text
    -> Convert.Tracks -> m (Maybe BlockId)
block source_key block_id ruler_id block_title tracks = do
    (dests, created) <- Ui.lookup_block block_id >>= \case
        Nothing -> do
            Ui.create_block (Id.unpack_id block_id) block_title
                [Block.track (Block.RId ruler_id) Config.ruler_width]
            return ([], True)
        Just exist -> do
            Ui.set_ruler_id block_id ruler_id
            case Map.lookup source_key (Block.block_integrated_manual exist) of
                Nothing -> Ui.throw $
                    "block to integrate already exists: " <> pretty block_id
                Just dests -> do
                    Ui.set_block_title block_id block_title
                    return (dests, False)
    new_dests <- Merge.merge_tracks Merge.ReplaceTitles block_id tracks dests
    Ui.set_integrated_manual block_id source_key (Just new_dests)
    when created $ do
        BlockConfig.toggle_merge_all block_id
    return $ if created then Just block_id else Nothing
