-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Implement a clipboard, and copy and paste from a selection.

    Who knew copy and paste was so complicated?  Copying is complicated because
    the structure isn't flat, i.e. a block has references to tracks and rulers.
    Pasting is complicated because the clipboard events have to be shifted and
    clipped according to the destination selection.

    Also, the tracks are typed in that it's not appropriate to paste control
    events into a note track.  However, I'm going to ignore that and assume the
    user won't paste them if he didn't mean it.

    Instead of having a special case clipboard, the clipboard is implemented
    as a normal block and tracks (rulers are not copied), in a clipboard
    namespace.  That way, you can have multiple clipboards by copying them to
    different clipboard namespaces, edit clipboards in place, and the paste
    code is the same as the code that merges another project from disk.

    Further ideas:

    - use two selections and a \"swap\" command

    - mouse chording for copy paste

    - different mouse buttons are hard to do on the mac, so use standard for
    now

    - merge with function... I think I can just do it at the REPL

    More complicated pastes should be implemented as derivers, which are more
    flexible than editing operations.  However, there could be a "derive in
    place" cmd to flatten deriver structure.
-}
module Cmd.Clip (
    clip_namespace
    , state_to_clip, clear_clip
    , cmd_cut_selection, cmd_copy_selection
    , cmd_paste_overwrite, cmd_paste_merge, cmd_paste_soft_merge
    , cmd_paste_insert, cmd_paste_stretch
#ifdef TESTING
    , state_to_namespace
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * clipboard ops

clip_namespace :: Id.Namespace
clip_namespace = Id.namespace Config.clip_namespace

clip_block_id :: BlockId
clip_block_id = Id.BlockId $ Id.id clip_namespace Config.clip_block_name

-- | Replace the clipboard with the given state.  This can be used to load
-- another score and use the clipboard as a staging area.
state_to_clip :: Cmd.M m => Ui.State -> m ()
state_to_clip state =
    reopen_views clip_namespace $ state_to_namespace state clip_namespace

clear_clip :: Cmd.M m => m ()
clear_clip = Transform.destroy_namespace clip_namespace

load_block_to_clip :: FilePath -> Cmd.CmdT IO ()
load_block_to_clip fn = state_to_clip =<< Simple.read_block fn

-- * copy

-- | Like 'cmd_copy_selection', but clear the selection after copying it.
cmd_cut_selection :: Cmd.M m => m ()
cmd_cut_selection = do
    cmd_copy_selection
    Edit.cmd_clear_selected

-- | Copy events under the current selection into the buffer.
cmd_copy_selection :: Cmd.M m => m ()
cmd_copy_selection = do
    selected <- get_selection =<< Selection.tracks
    reopen_views clip_namespace $ selected_to_block clip_block_id selected

-- | (track_title, events) pairs for each copied track within the copied
-- selection.
type Selected = [(Text, Events.Events)]

-- | Destroy the existing clip namespace and replace it with a new block
-- containing the contents of the Selected.
selected_to_block :: Ui.M m => BlockId -> Selected -> m ()
selected_to_block block_id selected = do
    let ns = Id.id_namespace (Id.unpack_id block_id)
    Transform.destroy_namespace ns
    block_id <- Ui.create_block (Id.unpack_id block_id) ""
        [Block.track (Block.RId Ui.no_ruler) 0]
    forM_ (zip [1..] selected) $ \(tracknum, (title, events)) ->
        Create.track_events block_id Ui.no_ruler tracknum Config.track_width
            (Track.track title
                (Events.map_events (Event.stack_ #= Nothing) events))

get_selection :: Cmd.M m => Selection.Tracks -> m Selected
get_selection (block_id, tracknums, _, range) = do
    tracks <- mapM Ui.get_track
        =<< mapMaybeM (Ui.event_track_at block_id) tracknums
    return $ map extract tracks
    where
    extract track =
        ( Track.track_title track
        , select_events range (Track.track_events track)
        )

select_events :: Events.Range -> Events.Events -> Events.Events
select_events range =
    Events.map_events (Event.start_ %= subtract (Events.range_start range))
    . Events.in_range range

-- * paste

-- $paste Insert events from the clipboard to the selection. If the selection
-- is a point it's the same as if it extended to the end of the block.

-- | The normal variety of paste that replaces the destination data.
cmd_paste_overwrite :: Cmd.M m => m ()
cmd_paste_overwrite = do
    (start, end, track_events) <- paste_info
    forM_ track_events $ \(track_id, events) -> do
        Ui.remove_events_range track_id (Events.Range start end)
        Ui.insert_events track_id events

cmd_paste_merge :: Cmd.M m => m ()
cmd_paste_merge = do
    (_, _, track_events) <- paste_info
    forM_ track_events $ \(track_id, events) ->
        Ui.insert_events track_id events

-- | Like 'cmd_paste_merge', except don't merge events that overlap with
-- existing ones.
cmd_paste_soft_merge :: Cmd.M m => m ()
cmd_paste_soft_merge = do
    (_, _, track_events) <- paste_info
    forM_ track_events $ \(track_id, events) -> do
        track_events <- Ui.get_events track_id
        Ui.insert_events track_id $
            filter (not . overlaps track_events) events
    where
    overlaps events event = Maybe.isJust $
        Events.overlapping (Event.start event) events

-- | Insert the events after pushing events after the selection down by
-- the inserted length, which is the minimum of the insert selection and the
-- length of the buffer.
cmd_paste_insert :: Cmd.M m => m ()
cmd_paste_insert = do
    (start, end, track_events) <- paste_info
    -- Only shift the tracks that are in clip_events.
    ruler_end <- Ui.block_ruler_end =<< Cmd.get_focused_block
    mapM_ (ModifyEvents.move_track_events ruler_end start (end-start) . fst)
        track_events
    forM_ track_events $ \(track_id, events) ->
        Ui.insert_events track_id events

-- | Paste the clipboard, but stretch or compress it to fit the selection.
cmd_paste_stretch :: Cmd.M m => m ()
cmd_paste_stretch = do
    (track_ids, clip_track_ids, start, end, _) <- get_paste_area
    events <- mapM Ui.get_events clip_track_ids
    let m_clip_s = Seq.minimum $ map Events.time_begin $
            filter (not . Events.null) events
        m_clip_e = Seq.maximum $ map Events.time_end $
            filter (not . Events.null) events
    case (m_clip_s, m_clip_e) of
        (Just clip_s, Just clip_e) -> do
            let stretched = map (stretch (start, end) (clip_s, clip_e)
                    . Events.ascending) events
            forM_ (zip track_ids stretched) $ \(track_id, stretched) -> do
                Ui.remove_events_range track_id (Events.Range start end)
                Ui.insert_events track_id stretched
        _ -> return ()

stretch :: (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime)
    -> [Event.Event] -> [Event.Event]
stretch (start, end) (clip_s, clip_e) = map reposition
    where
    reposition = (Event.start_ %= (\pos -> (pos-clip_s) * factor + start))
        . (Event.duration_ %= (*factor))
    factor = (end - start) / (clip_e - clip_s)


-- * implementation

-- ** copy

-- | Rename the blocks and tracks in the given state into the given namespace
-- and replace the IDs already in that namespace with it.  Rulers are ignored.
--
-- This means that if the given state has IDs in more than one namespace, they
-- will be flattened into one.  Any collisions will throw an exception.
state_to_namespace :: Ui.M m => Ui.State -> Id.Namespace -> m ()
state_to_namespace state ns = do
    state <- set_namespace ns state
    Transform.destroy_namespace ns
    global_st <- Ui.get
    merged <- Ui.require_right (("merge states: "<>) . showt)
        (Transform.merge_states global_st state)
    Ui.put merged

reopen_views :: Ui.M m => Id.Namespace -> m a -> m a
reopen_views ns operation = do
    views <- map snd . filter ((==ns) . Id.ident_namespace . fst) <$>
        Ui.gets (Map.toList . Ui.state_views)
    result <- operation
    block_ids <- Ui.all_block_ids
    let reopen = filter ((`elem` block_ids) . Block.view_block) views
    forM_ reopen $ \view ->
        Create.sized_view (Block.view_block view) (Block.view_rect view)
    return result

-- | Set all the IDs in the state to be in the given namespace, except rulers.
-- Collisions will throw.  Rulers are omitted because copy and paste doesn't
-- mess with rulers.
set_namespace :: Ui.M m => Id.Namespace -> Ui.State -> m Ui.State
set_namespace ns state = do
    let state2 = state { Ui.state_rulers = Map.empty }
    Ui.require_right (("set to clip namespace: "<>) . showt) $
        Ui.exec state2 $ do
            Transform.map_view_ids (Id.set_namespace ns)
            Transform.map_block_ids (Id.set_namespace ns)
            Transform.map_track_ids (Id.set_namespace ns)

-- ** paste

-- | Get the info necessary to paste from the clipboard: start and end pos,
-- the tracks in the destination selection, and the events from the clipboard
-- paired with the track it should go into.  The clipboard events are truncated
-- to start--end and shifted into the paste range.
paste_info :: Cmd.M m => m (ScoreTime, ScoreTime, [(TrackId, [Event.Event])])
paste_info = do
    (track_ids, clip_track_ids, start, sel_end, event_end) <- get_paste_area
    tracks <- mapM Ui.get_track clip_track_ids
    let clip_and_move = map (Event.start_ %= (+start))
            . clip_to_selection start event_end
            . Events.ascending . Track.track_events
    return (start, sel_end, zip track_ids (map clip_and_move tracks))

clip_to_selection :: ScoreTime -> ScoreTime -> [Event.Event] -> [Event.Event]
clip_to_selection start end
    -- A point selection should be able to paste a 0 dur event.
    | start == end = maybe [] (:[])
        . List.find (\e -> Event.start e == 0 && Event.duration e == 0)
    | otherwise = Events.clip_list False (end - start)

-- | Get the destination and clip tracks involved in a paste, along with the
-- paste selection.
--
-- During pastes, a point selection extends to the end of the last pasted
-- event.  However, the paste range is limited to the end of the ruler on the
-- block.  Otherwise, it's easy to paste events past the end of the block,
-- which are then difficult to edit.
get_paste_area :: Cmd.M m =>
    m ([TrackId], [TrackId], ScoreTime, ScoreTime, ScoreTime)
get_paste_area = do
    (block_id, tracknums, track_ids, range) <- Selection.tracks
    let (start, end) = Events.range_times range
    ruler_end <- Ui.block_ruler_end block_id
    clip_block <- Ui.get_block clip_block_id
    -- If the clip block has any rulers or anything, I skip them.
    let clip_track_ids =
            take (length tracknums) (Block.block_track_ids clip_block)
    clip_end <- Ui.block_event_end clip_block_id
    -- If start==end, I have to set the end past the end of the clip in case
    -- the last event has dur 0.
    return (track_ids, clip_track_ids, start,
        min ruler_end (if start == end then start + clip_end else end),
        if start == end then ruler_end else end)
