{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Block level cmds.
module Cmd.Lang.LBlock where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import Util.Control
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.PitchTrack as PitchTrack
import qualified Cmd.Selection as Selection

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve

import Types


-- * doc

doc :: Cmd.CmdL Text.Text
doc = CallDoc.doc_text <$> track_doc

-- | Write HTML documentation for the selected track to
-- @build/derive_doc.html@.
html_doc :: Cmd.CmdL ()
html_doc = do
    doc <- track_doc
    liftIO $ Text.IO.writeFile "build/derive_doc.html"
        (CallDoc.doc_html doc)

track_doc :: Cmd.CmdL CallDoc.Document
track_doc = do
    (block_id, _, track_id, _) <- Selection.get_insert
    CallDoc.track block_id track_id

-- * block call

-- | Rename a block and all occurrances in the current block.
--
-- It doesn't update TrackIds so they may still be named under their old block,
-- but track id names aren't supposed to carry meaning anyway.
rename :: BlockId -> BlockId -> Cmd.CmdL ()
rename = Create.rename_block

-- | Rename block calls in a single block.
replace :: BlockId -> BlockId -> Cmd.CmdL ()
replace from to = do
    block_id <- Cmd.get_focused_block
    ModifyEvents.block block_id $ ModifyEvents.text $
        replace_block_call from to

replace_block_call :: BlockId -> BlockId -> String -> String
replace_block_call from to text
    | text == Id.ident_name from = Id.ident_name to
    | text == Id.ident_string from = Id.ident_string to
    | otherwise = text

-- * create

-- | If the events under the cursor are a block calls, create blocks that don't
-- already exist.  Optionally use a template block.
block_for_event :: Maybe BlockId -> Cmd.CmdL ()
block_for_event template = mapM_ make =<< Selection.events
    where
    make (_, _, events) = mapM_
        (create_named template . Event.event_string) events

create_named :: Maybe BlockId -> String -> Cmd.CmdL ()
create_named template name = whenM (can_create name) $
    Create.view =<< case template of
        Nothing -> do
            template_id <- Cmd.get_focused_block
            Create.named_block name =<< State.block_ruler template_id
        Just template_id -> Create.named_block_from_template template_id name

can_create :: (State.M m) => String -> m Bool
can_create "" = return False
can_create name = do
    ns <- State.get_namespace
    case Types.BlockId <$> Id.read_short ns name of
        Just block_id -> not . Map.member block_id
            <$> State.gets State.state_blocks
        Nothing -> return False

-- * dividers

-- | Insert a divider to the right of the selection.
divide :: Cmd.CmdL ()
divide = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    State.insert_track block_id (tracknum+1) Block.divider

-- | Remove a divider to the right of the selection.  The selection likes to
-- skip dividers so they can't be deleted normally.
undivide :: Cmd.CmdL ()
undivide = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    tracks <- Block.block_tracks <$> State.get_block block_id
    let found = List.find ((==Block.divider) . snd)
            (drop tracknum (zip [0..] tracks))
    when_just found $ \(n, _) -> State.remove_track block_id n

collapse_children :: (Cmd.M m) => m ()
collapse_children = do
    (block_id, _, track_id, _) <- Selection.get_insert
    BlockConfig.collapse_children block_id track_id

expand_children :: (Cmd.M m) => m ()
expand_children = do
    (block_id, _, track_id, _) <- Selection.get_insert
    BlockConfig.expand_children block_id track_id

-- * merge

append :: (Cmd.M m) => BlockId -> m ()
append source = do
    dest <- Cmd.get_focused_block
    BlockConfig.append dest source

create_merged :: (Cmd.M m) => BlockId -> BlockId -> m ViewId
create_merged b1 b2 = do
    ruler_id <- State.block_ruler b1
    new <- Create.block ruler_id
    BlockConfig.append new b1
    BlockConfig.append new b2
    Create.view new

-- * stretch

stretch_block :: ScoreTime -> BlockId -> Cmd.CmdL ()
stretch_block factor block_id = ModifyEvents.block block_id $
    ModifyEvents.event (stretch factor)

stretch :: ScoreTime -> ModifyEvents.Event
stretch factor = Event.move (*factor) . Event.modify_duration (*factor)

-- * pitch

simplify_block_enharmonics :: BlockId -> Cmd.CmdL ()
simplify_block_enharmonics block_id =
    ModifyEvents.block block_id simplify_enharmonics

-- | This only works for Twelve at the moment.  For it to work for any scale
-- I need a way to parse to Theory.Pitch.  Can't use scale_enharmonics because
-- I don't want to mess with ones that are already simple.
simplify_enharmonics :: (Cmd.M m) => ModifyEvents.Track m
simplify_enharmonics = PitchTrack.pitch_tracks $ \scale key note ->
    case Twelve.read_pitch note of
        Left _ -> Right note
        Right pitch
            | abs (Theory.pitch_accidentals pitch) < 2 -> Right note
            | otherwise -> case Scale.scale_enharmonics scale key note of
                Right (simpler : _) -> Right simpler
                _ -> Right note
