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
    clipNamespace
    , stateToClip, clearClip
    , cmdCutSelection, cmdCopySelection
    , cmdPasteOverwrite, cmdPasteMerge, cmdPasteSoftMerge
    , cmdPasteInsert, cmdPasteStretch
#ifdef TESTING
    , stateToNamespace
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists
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

clipNamespace :: Id.Namespace
clipNamespace = Id.namespace Config.clipNamespace

clipBlockId :: BlockId
clipBlockId = Id.BlockId $ Id.id clipNamespace Config.clipBlockName

-- | Replace the clipboard with the given state.  This can be used to load
-- another score and use the clipboard as a staging area.
stateToClip :: Cmd.M m => Ui.State -> m ()
stateToClip state =
    reopenViews clipNamespace $ stateToNamespace state clipNamespace

clearClip :: Cmd.M m => m ()
clearClip = Transform.destroyNamespace clipNamespace

loadBlockToClip :: FilePath -> Cmd.CmdT IO ()
loadBlockToClip fn = stateToClip =<< Simple.readBlock fn

-- * copy

-- | Like 'cmd_copy_selection', but clear the selection after copying it.
cmdCutSelection :: Cmd.M m => m ()
cmdCutSelection = do
    cmdCopySelection
    Edit.cmdClearSelected

-- | Copy events under the current selection into the buffer.
cmdCopySelection :: Cmd.M m => m ()
cmdCopySelection = do
    selected <- getSelection =<< Selection.tracks
    reopenViews clipNamespace $ selectedToBlock clipBlockId selected

-- | (track_title, events) pairs for each copied track within the copied
-- selection.
type Selected = [(Text, Events.Events)]

-- | Destroy the existing clip namespace and replace it with a new block
-- containing the contents of the Selected.
selectedToBlock :: Ui.M m => BlockId -> Selected -> m ()
selectedToBlock blockId selected = do
    let ns = Id.idNamespace (Id.unpackId blockId)
    Transform.destroyNamespace ns
    blockId <- Ui.createBlock (Id.unpackId blockId) ""
        [Block.track (Block.RId Ui.noRuler) 0]
    forM_ (zip [1..] selected) $ \(tracknum, (title, events)) ->
        Create.trackEvents blockId Ui.noRuler tracknum Config.trackWidth
            (Track.track title
                (Events.mapEvents (Event.stack_ #= Nothing) events))

getSelection :: Cmd.M m => Selection.Tracks -> m Selected
getSelection (blockId, tracknums, _, range) = do
    tracks <- mapM Ui.getTrack
        =<< mapMaybeM (Ui.eventTrackAt blockId) tracknums
    pure $ map extract tracks
    where
    extract track =
        ( Track.trackTitle track
        , selectEvents range (Track.trackEvents track)
        )

selectEvents :: Events.Range -> Events.Events -> Events.Events
selectEvents range =
    Events.mapEvents (Event.start_ %= subtract (Events.rangeStart range))
    . Events.inRange range

-- * paste

-- $paste Insert events from the clipboard to the selection. If the selection
-- is a point it's the same as if it extended to the end of the block.

-- | The normal variety of paste that replaces the destination data.
cmdPasteOverwrite :: Cmd.M m => m ()
cmdPasteOverwrite = do
    (start, end, trackEvents) <- pasteInfo
    forM_ trackEvents $ \(trackId, events) -> do
        Ui.removeEventsRange trackId (Events.Range start end)
        Ui.insertEvents trackId events

cmdPasteMerge :: Cmd.M m => m ()
cmdPasteMerge = do
    (_, _, trackEvents) <- pasteInfo
    forM_ trackEvents $ \(trackId, events) ->
        Ui.insertEvents trackId events

-- | Like 'cmd_paste_merge', except don't merge events that overlap with
-- existing ones.
cmdPasteSoftMerge :: Cmd.M m => m ()
cmdPasteSoftMerge = do
    (_, _, trackEvents) <- pasteInfo
    forM_ trackEvents $ \(trackId, events) -> do
        trackEvents <- Ui.getEvents trackId
        Ui.insertEvents trackId $
            filter (not . overlaps trackEvents) events
    where
    overlaps events event = Maybe.isJust $
        Events.overlapping (Event.start event) events

-- | Insert the events after pushing events after the selection down by
-- the inserted length, which is the minimum of the insert selection and the
-- length of the buffer.
cmdPasteInsert :: Cmd.M m => m ()
cmdPasteInsert = do
    (start, end, trackEvents) <- pasteInfo
    -- Only shift the tracks that are in clip_events.
    rulerEnd <- Ui.blockRulerEnd =<< Cmd.getFocusedBlock
    mapM_ (ModifyEvents.moveTrackEvents rulerEnd start (end-start) . fst)
        trackEvents
    forM_ trackEvents $ \(trackId, events) ->
        Ui.insertEvents trackId events

-- | Paste the clipboard, but stretch or compress it to fit the selection.
cmdPasteStretch :: Cmd.M m => m ()
cmdPasteStretch = do
    (trackIds, clipTrackIds, start, end, _) <- getPasteArea
    events <- mapM Ui.getEvents clipTrackIds
    let mClipS = Lists.minimum $ map Events.timeBegin $
            filter (not . Events.null) events
        mClipE = Lists.maximum $ map Events.timeEnd $
            filter (not . Events.null) events
    case (mClipS, mClipE) of
        (Just clipS, Just clipE) -> do
            let stretched = map (stretch (start, end) (clipS, clipE)
                    . Events.ascending) events
            forM_ (zip trackIds stretched) $ \(trackId, stretched) -> do
                Ui.removeEventsRange trackId (Events.Range start end)
                Ui.insertEvents trackId stretched
        _ -> pure ()

stretch :: (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime)
    -> [Event.Event] -> [Event.Event]
stretch (start, end) (clipS, clipE) = map reposition
    where
    reposition = (Event.start_ %= (\pos -> (pos-clipS) * factor + start))
        . (Event.duration_ %= (*factor))
    factor = (end - start) / (clipE - clipS)


-- * implementation

-- ** copy

-- | Rename the blocks and tracks in the given state into the given namespace
-- and replace the IDs already in that namespace with it.  Rulers are ignored.
--
-- This means that if the given state has IDs in more than one namespace, they
-- will be flattened into one.  Any collisions will throw an exception.
stateToNamespace :: Ui.M m => Ui.State -> Id.Namespace -> m ()
stateToNamespace state ns = do
    state <- setNamespace ns state
    Transform.destroyNamespace ns
    globalSt <- Ui.get
    merged <- Ui.requireRight (("merge states: "<>) . showt)
        (Transform.mergeStates globalSt state)
    Ui.put merged

reopenViews :: Ui.M m => Id.Namespace -> m a -> m a
reopenViews ns operation = do
    views <- map snd . filter ((==ns) . Id.identNamespace . fst) <$>
        Ui.gets (Map.toList . Ui.stateViews)
    result <- operation
    blockIds <- Ui.allBlockIds
    let reopen = filter ((`elem` blockIds) . Block.viewBlock) views
    forM_ reopen $ \view ->
        Create.sizedView (Block.viewBlock view) (Block.viewRect view)
    pure result

-- | Set all the IDs in the state to be in the given namespace, except rulers.
-- Collisions will throw.  Rulers are omitted because copy and paste doesn't
-- mess with rulers.
setNamespace :: Ui.M m => Id.Namespace -> Ui.State -> m Ui.State
setNamespace ns state = do
    let state2 = state { Ui.stateRulers = Map.empty }
    Ui.requireRight (("set to clip namespace: "<>) . showt) $
        Ui.exec state2 $ do
            Transform.mapViewIds (Id.setNamespace ns)
            Transform.mapBlockIds (Id.setNamespace ns)
            Transform.mapTrackIds (Id.setNamespace ns)

-- ** paste

-- | Get the info necessary to paste from the clipboard: start and end pos,
-- the tracks in the destination selection, and the events from the clipboard
-- paired with the track it should go into.  The clipboard events are truncated
-- to start--end and shifted into the paste range.
pasteInfo :: Cmd.M m => m (ScoreTime, ScoreTime, [(TrackId, [Event.Event])])
pasteInfo = do
    (trackIds, clipTrackIds, start, selEnd, eventEnd) <- getPasteArea
    tracks <- mapM Ui.getTrack clipTrackIds
    let clipAndMove = map (Event.start_ %= (+start))
            . clipToSelection start eventEnd
            . Events.ascending . Track.trackEvents
    pure (start, selEnd, zip trackIds (map clipAndMove tracks))

clipToSelection :: ScoreTime -> ScoreTime -> [Event.Event] -> [Event.Event]
clipToSelection start end
    -- A point selection should be able to paste a 0 dur event.
    | start == end = maybe [] (:[])
        . List.find (\e -> Event.start e == 0 && Event.duration e == 0)
    | otherwise = Events.clipList False (end - start)

-- | Get the destination and clip tracks involved in a paste, along with the
-- paste selection.
--
-- During pastes, a point selection extends to the end of the last pasted
-- event.  However, the paste range is limited to the end of the ruler on the
-- block.  Otherwise, it's easy to paste events past the end of the block,
-- which are then difficult to edit.
getPasteArea :: Cmd.M m =>
    m ([TrackId], [TrackId], ScoreTime, ScoreTime, ScoreTime)
getPasteArea = do
    (blockId, tracknums, trackIds, range) <- Selection.tracks
    let (start, end) = Events.rangeTimes range
    rulerEnd <- Ui.blockRulerEnd blockId
    clipBlock <- Ui.getBlock clipBlockId
    -- If the clip block has any rulers or anything, I skip them.
    let clipTrackIds =
            take (length tracknums) (Block.blockTrackIds clipBlock)
    clipEnd <- Ui.blockEventEnd clipBlockId
    -- If start==end, I have to set the end past the end of the clip in case
    -- the last event has dur 0.
    pure (trackIds, clipTrackIds, start,
        min rulerEnd (if start == end then start + clipEnd else end),
        if start == end then rulerEnd else end)
