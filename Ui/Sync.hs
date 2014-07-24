-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Take Updates, which are generated by 'Ui.Diff', and send them to the UI.

    The C++ level and BlockC have no notion of "blocks" which may be shared
    between block views.  The haskell State does have this notion, so it's this
    module's job to distribute an operation on a block to all of the C++ block
    views that are displaying that block.

    So if this module has a bug, two views of one block could get out of sync
    and display different data.  Hopefully that won't happen.

    Implementation of merged tracks:

    They need to be implemented in two places: 1. when a block is updated with
    changed merged tracks, and 2. when a track is updated they should be
    preserved.  It's tricky because unlike normal track events, they are block
    level, not track level, so the same track in different blocks may be merged
    with different events.  I don't actually see a lot of use-case for the same
    track in different blocks, but as long as I have it, it makes sense that it
    can have different merges in different blocks, since it's really
    a display-level effect.

    This is a hassle because case 1 has to go hunt down the event info and case
    2 has to go hunt down the per-block info, but such is life.
-}
module Ui.Sync (
    sync
    , set_track_signals
    , set_play_position, clear_play_position, set_highlights, clear_highlights
    , edit_input
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Derive.ParseTitle as ParseTitle
import qualified App.Config as Config
import Types


-- | Sync with the ui by applying the given updates to it.
--
-- TrackSignals are passed separately instead of going through diff because
-- they're special: they exist in Cmd.State and not in Ui.State.  It's rather
-- unpleasant, but as long as it's only TrackSignals then I can deal with it.
sync :: Ui.Channel -> Track.TrackSignals -> Track.SetStyleHigh -> State.State
    -> [Update.DisplayUpdate] -> IO (Maybe State.Error)
sync ui_chan track_signals set_style state updates = do
    updates <- check_updates state $
        Update.sort (Update.collapse_updates updates)
    result <- State.run state $
        do_updates ui_chan track_signals set_style updates
    return $ case result of
        Left err -> Just err
        -- I reuse State.StateT for convenience, but run_update should
        -- not modify the State and hence shouldn't produce any updates.
        -- TODO Try to split StateT into ReadStateT and ReadWriteStateT to
        -- express this in the type?
        Right _ -> Nothing

-- | Filter out updates that will cause the BlockC level to throw an exception,
-- and log an error instead.  BlockC could log itself, but if BlockC gets a bad
-- update then that indicates a bug in the program, while this is meant to
-- filter out updates that could occur \"normally\".  I'm not sure the
-- distinction is worth it.
check_updates :: State.State -> [Update.DisplayUpdate]
    -> IO [Update.DisplayUpdate]
check_updates state = filterM $ \update -> case update of
    Update.View view_id u -> case u of
        -- Already destroyed, so I don't expect it to exist.
        Update.DestroyView -> return True
        _ | view_id `Map.member` State.state_views state -> return True
        _ -> do
            Log.warn $ "Update for nonexistent " <> show view_id <> ": "
                <> pretty u
            return False
    _ -> return True

do_updates :: Ui.Channel -> Track.TrackSignals -> Track.SetStyleHigh
    -> [Update.DisplayUpdate] -> State.StateT IO ()
do_updates ui_chan track_signals set_style updates = do
    -- Debug.fullM Debug.putp "sync updates" updates
    actions <- mapM (run_update track_signals set_style) updates
    liftIO $ Ui.send_action ui_chan (sequence_ actions)

set_track_signals :: Ui.Channel -> BlockId -> State.State -> Track.TrackSignals
    -> IO ()
set_track_signals ui_chan block_id state track_signals =
    case State.eval state rendering_tracks of
        Left err ->
            -- This could happen if track_signals had a stale track_id.  That
            -- could happen if I deleted a track before the deriver came back
            -- with its signal.
            -- TODO but I should just filter out the bad track_id in that case
            Log.warn $ "getting tracknums of track_signals: " ++ show err
        Right tracks -> do
            let tsigs = do
                    (view_id, track_id, tracknum) <- tracks
                    Just tsig <- [Map.lookup (block_id, track_id) track_signals]
                    return (view_id, tracknum, tsig)
            -- Make sure tsigs is fully forced, because a hang on the fltk
            -- event loop can be confusing.
            tsigs `DeepSeq.deepseq` Ui.send_action ui_chan $
                forM_ tsigs $ \(view_id, tracknum, tsig) ->
                    set_track_signal view_id tracknum tsig
    where
    -- | Get the tracks of this block which want to render a signal.
    rendering_tracks :: State.StateId [(ViewId, TrackId, TrackNum)]
    rendering_tracks = do
        view_ids <- Map.keys <$> State.views_of block_id
        blocks <- mapM State.block_of view_ids
        btracks <- mapM get_tracks blocks
        return $ do
            (view_id, tracks) <- zip view_ids btracks
            ((tracknum, track_id, flags), track) <- tracks
            guard (wants_tsig flags track)
            return (view_id, track_id, tracknum)
    get_tracks block = zip triples <$> mapM State.get_track track_ids
        where
        track_ids = [tid | (_, tid, _) <- triples]
        triples = [(tracknum, tid, Block.track_flags track) |
            (tracknum,
                track@(Block.Track { Block.tracklike_id = Block.TId tid _ }))
            <- zip [0..] (Block.block_tracks block)]

set_track_signal :: ViewId -> TrackNum -> Track.TrackSignal -> IO ()
#ifdef TESTING
-- ResponderTest using tests wind up calling this via set_track_signals, which
-- is out of band so it bypasses Responder.state_sync, and winds up segfaulting
-- on OS X.
set_track_signal _ _ _ = return ()
#else
set_track_signal = BlockC.set_track_signal
#endif

-- | The play position selection bypasses all the usual State -> Diff -> Sync
-- stuff for a direct write to the UI.
--
-- This is because it happens asynchronously and would be noisy and inefficient
-- to work into the responder loop, and isn't part of the usual state that
-- should be saved anyway.
set_play_position :: Ui.Channel -> [(ViewId, [(TrackNum, ScoreTime)])] -> IO ()
set_play_position chan block_sels = unless (null block_sels) $
    Ui.send_action chan $ sequence_ $ do
        (view_id, tracknum_pos) <- Seq.group_fst block_sels
        (tracknums, pos) <- Seq.group_snd (concat tracknum_pos)
        return $ BlockC.set_selection False view_id
            Config.play_position_selnum tracknums
            [BlockC.Selection Config.play_selection_color pos pos True]

clear_play_position :: Ui.Channel -> ViewId -> IO ()
clear_play_position = clear_selections Config.play_position_selnum

type Range = (TrackTime, TrackTime)

set_highlights :: Ui.Channel -> [((ViewId, TrackNum), (Range, Color.Color))]
    -> IO ()
set_highlights chan view_sels = unless (null view_sels) $
    Ui.send_action chan $ sequence_ $ do
        (view_id, tracknum_sels) <- group_by_view view_sels
        (tracknum, range_colors) <- tracknum_sels
        return $ BlockC.set_selection False view_id Config.highlight_selnum
            [tracknum] (map make_sel range_colors)
    where
    make_sel ((start, end), color) = BlockC.Selection color start end False

-- | Juggle the selections around into the format that 'BlockC.set_selection'
-- wants.
group_by_view :: [((ViewId, TrackNum), (Range, Color.Color))]
    -> [(ViewId, [(TrackNum, [(Range, Color.Color)])])]
group_by_view view_sels = map (second Seq.group_fst) by_view
    where
    (view_tracknums, range_colors) = unzip view_sels
    (view_ids, tracknums) = unzip view_tracknums
    by_view :: [(ViewId, [(TrackNum, (Range, Color.Color))])]
    by_view = Seq.group_fst $ zip view_ids (zip tracknums range_colors)

clear_highlights :: Ui.Channel -> ViewId -> IO ()
clear_highlights = clear_selections Config.highlight_selnum

clear_selections :: Types.SelNum -> Ui.Channel -> ViewId -> IO ()
clear_selections selnum chan view_id = Ui.send_action chan $ do
    tracks <- BlockC.tracks view_id
    BlockC.set_selection False view_id selnum [0 .. tracks - 1] []

edit_input :: State.State -> Cmd.EditInput -> IO ()
edit_input _ (Cmd.EditOpen view_id tracknum at text selection) =
    BlockC.edit_open view_id tracknum at text selection
edit_input state (Cmd.EditInsert text) =
    BlockC.edit_insert (Map.keys (State.state_views state)) text


-- * run_update

-- There's a fair amount of copy and paste in here, since CreateView subsumes
-- the functions of InsertTrack and many others.  For example, the merged
-- events of a given track are calculated in 4 separate places.  It's nasty
-- error-prone imperative code.  I'd like to factor it better but I don't know
-- how.
--
-- Also, set_style occurs in a lot of places and has to be transformed in the
-- same way every time.  The problem is that each case has a large and
-- overlapping set of required data, and it comes from different places.  Also
-- work is expressed in multiple places, e.g. CreateView contains all the stuff
-- from modifying views and creating tracks.
--
-- It's also a little confusing in that this function runs in StateT, but
-- returns an IO action to be run in the UI thread, so there are two monads
-- here.

-- | Generate an IO action that applies the update to the UI.
--
-- CreateView Updates will modify the State to add the ViewPtr.  The IO in
-- the StateT is needed only for some logging.
run_update :: Track.TrackSignals -> Track.SetStyleHigh -> Update.DisplayUpdate
    -> State.StateT IO (IO ())
run_update track_signals set_style update = case update of
    Update.View view_id update ->
        update_view track_signals set_style view_id update
    Update.Block block_id update ->
        update_block track_signals set_style block_id update
    Update.Track track_id update ->
        update_track track_signals set_style track_id update
    Update.Ruler ruler_id ->
        update_ruler track_signals set_style ruler_id
    Update.State () -> return (return ())

update_view :: Track.TrackSignals -> Track.SetStyleHigh -> ViewId
    -> Update.View -> State.StateT IO (IO ())
update_view track_signals set_style view_id Update.CreateView = do
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)

    let dtracks = map Block.display_track (Block.block_tracks block)
        btracks = Block.block_tracks block
        tlike_ids = map Block.tracklike_id btracks
    -- It's important to get the tracklikes from the dtracks, not the
    -- tlike_ids.  That's because the dtracks will have already turned
    -- Collapsed tracks into Dividers.
    tracklikes <- mapM (State.get_tracklike . Block.dtracklike_id) dtracks

    let sels = Block.view_selections view
        selnum_sels :: [(Types.SelNum, [([TrackNum], [BlockC.Selection])])]
        selnum_sels =
            [ (selnum, track_selections selnum (length btracks) (Just sel))
            | (selnum, sel) <- Map.toAscList sels
            ]
    state <- State.get
    -- I manually sync the new empty view with its state.  It might reduce
    -- repetition to let Diff.diff do that by diffing against a state with an
    -- empty view, but this way seems less complicated if more error-prone.
    -- Sync: title, tracks, selection, skeleton
    return $ do
        let title = block_window_title
                (State.config_namespace (State.state_config state))
                view_id (Block.view_block view)
        BlockC.create_view view_id title (Block.view_rect view)
            (Block.block_config block)
        forM_ (List.zip5 [0..] dtracks btracks tlike_ids tracklikes) $
            \(tracknum, dtrack, btrack, tlike_id, tlike) ->
                insert_track state set_style (Block.view_block view) view_id
                    tracknum dtrack tlike_id tlike track_signals
                    (Block.track_flags btrack)
        unless (Text.null (Block.block_title block)) $
            BlockC.set_title view_id (Block.block_title block)
        BlockC.set_skeleton view_id (Block.block_skeleton block)
            (Block.integrate_skeleton block) (map Block.dtrack_status dtracks)
        forM_ selnum_sels $ \(selnum, tracknums_sels) ->
            forM_ tracknums_sels $ \(tracknums, sels) ->
                BlockC.set_selection True view_id selnum tracknums sels
        BlockC.set_status view_id (Block.show_status (Block.view_status view))
            (Block.status_color (Block.view_block view) block
                (State.config_root (State.state_config state)))
        BlockC.set_zoom view_id (Block.view_zoom view)
        BlockC.set_track_scroll view_id (Block.view_track_scroll view)

update_view _ _ view_id update = case update of
    -- The previous equation matches CreateView, but ghc warning doesn't
    -- figure that out.
    Update.CreateView -> error "run_update: notreached"
    Update.DestroyView -> return $ BlockC.destroy_view view_id
    Update.ViewSize rect -> return $ BlockC.set_size view_id rect
    Update.Status status color ->
        return $ BlockC.set_status view_id (Block.show_status status) color
    Update.TrackScroll offset ->
        return $ BlockC.set_track_scroll view_id offset
    Update.Zoom zoom -> return $ BlockC.set_zoom view_id zoom
    Update.Selection selnum maybe_sel -> return $ do
        tracks <- BlockC.tracks view_id
        let tracknums_sels = track_selections selnum tracks maybe_sel
        forM_ tracknums_sels $ \(tracknums, sels) ->
            BlockC.set_selection True view_id selnum tracknums sels
    Update.BringToFront -> return $ BlockC.bring_to_front view_id
    Update.TitleFocus tracknum ->
        return $ maybe (BlockC.set_block_title_focus view_id)
            (BlockC.set_track_title_focus view_id) tracknum

-- | Block ops apply to every view with that block.
update_block :: Track.TrackSignals -> Track.SetStyleHigh
    -> BlockId -> Update.Block Block.DisplayTrack -> State.StateT IO (IO ())
update_block track_signals set_style block_id update = do
    view_ids <- fmap Map.keys (State.views_of block_id)
    case update of
        Update.BlockTitle title -> return $
            mapM_ (flip BlockC.set_title title) view_ids
        Update.BlockConfig config -> return $
            mapM_ (flip BlockC.set_model_config config) view_ids
        Update.BlockSkeleton skel integrate_edges statuses -> return $
            forM_ view_ids $ \view_id ->
                BlockC.set_skeleton view_id skel integrate_edges statuses
        Update.RemoveTrack tracknum -> return $
            mapM_ (flip BlockC.remove_track tracknum) view_ids
        Update.InsertTrack tracknum dtrack ->
            create_track view_ids tracknum dtrack
        Update.BlockTrack tracknum dtrack -> do
            tracklike <- State.get_tracklike (Block.dtracklike_id dtrack)
            state <- State.get
            let set_style_low = update_set_style state block_id
                    (Block.dtracklike_id dtrack) set_style
            return $ forM_ view_ids $ \view_id -> do
                BlockC.set_display_track view_id tracknum dtrack
                let merged = events_of_track_ids state
                        (Block.dtrack_merged dtrack)
                -- This is unnecessary if I just collapsed the track, but
                -- no big deal.
                BlockC.update_entire_track False view_id tracknum tracklike
                    merged set_style_low
    where
    create_track view_ids tracknum dtrack = do
        let tlike_id = Block.dtracklike_id dtrack
        tlike <- State.get_tracklike tlike_id
        state <- State.get
        -- I need to get this for wants_tsig.
        mb_btrack <- fmap (\b -> Seq.at (Block.block_tracks b) tracknum)
            (State.get_block block_id)
        flags <- case mb_btrack of
            Nothing -> do
                liftIO $ Log.warn $
                    "InsertTrack with tracknum that's not in the block's "
                    ++ "tracks: " ++ show update
                return mempty
            Just btrack -> return (Block.track_flags btrack)
        return $ forM_ view_ids $ \view_id ->
            insert_track state set_style block_id view_id tracknum dtrack
                tlike_id tlike track_signals flags

update_track :: Track.TrackSignals -> Track.SetStyleHigh
    -> TrackId -> Update.Track -> State.StateT IO (IO ())
update_track _ set_style track_id update = do
    block_ids <- map fst <$> dtracks_with_track_id track_id
    state <- State.get
    acts <- forM block_ids $ \block_id -> do
        block <- State.get_block block_id
        view_ids <- fmap Map.keys (State.views_of block_id)
        forM (tracklikes track_id block) $ \(tracknum, tracklike_id) -> do
            let merged = merged_events_of state block tracknum
            tracklike <- State.get_tracklike tracklike_id
            let set_style_low = update_set_style state block_id tracklike_id
                    set_style
            forM view_ids $ \view_id ->
                track_update set_style_low view_id tracklike tracknum merged
                    update
    return (sequence_ (concat (concat acts)))
    where
    tracklikes track_id block =
        [ (n, track) | (n, track@(Block.TId tid _)) <- zip [0..] tracks
        , tid == track_id
        ]
        where
        tracks = map Block.dtracklike_id (Block.block_display_tracks block)
    track_update set_style view_id tracklike tracknum merged update =
        case update of
            Update.TrackEvents low high -> return $
                BlockC.update_track False view_id tracknum tracklike merged
                    set_style low high
            Update.TrackAllEvents -> return $
                BlockC.update_entire_track False view_id tracknum tracklike
                    merged set_style
            Update.TrackTitle title -> return $
                BlockC.set_track_title view_id tracknum title
            Update.TrackBg _color ->
                -- update_track also updates the bg color
                return $ BlockC.update_track False view_id tracknum tracklike
                    merged set_style 0 0
            Update.TrackRender _render -> return $
                BlockC.update_entire_track False view_id tracknum tracklike
                    merged set_style

update_ruler :: Track.TrackSignals -> Track.SetStyleHigh
    -> RulerId -> State.StateT IO (IO ())
update_ruler _ set_style ruler_id = do
    blocks <- dtracks_with_ruler_id ruler_id
    state <- State.get
    let tinfo = [(block_id, tracknum, tid)
            | (block_id, tracks) <- blocks, (tracknum, tid) <- tracks]
    fmap sequence_ $ forM tinfo $ \(block_id, tracknum, tracklike_id) -> do
        view_ids <- fmap Map.keys (State.views_of block_id)
        tracklike <- State.get_tracklike tracklike_id
        block <- State.get_block block_id
        let merged = merged_events_of state block tracknum
        return $ sequence_ $ flip map view_ids $ \view_id ->
            BlockC.update_entire_track True view_id tracknum tracklike merged
                (update_set_style state block_id tracklike_id set_style)

-- ** util

-- | Insert a track.  Tracks require a crazy amount of configuration.
insert_track :: State.State -> Track.SetStyleHigh -> BlockId -> ViewId
    -> TrackNum -> Block.DisplayTrack -> Block.TracklikeId -> Block.Tracklike
    -> Track.TrackSignals -> Set.Set Block.TrackFlag -> IO ()
insert_track state set_style block_id view_id tracknum dtrack tlike_id tlike
        track_signals flags = do
    BlockC.insert_track view_id tracknum tlike merged set_style_low
        (Block.dtrack_width dtrack)
    BlockC.set_display_track view_id tracknum dtrack
    case (tlike, tlike_id) of
        (Block.T t _, Block.TId tid _) -> do
            unless (Text.null (Track.track_title t)) $
                BlockC.set_track_title view_id tracknum (Track.track_title t)
            case Map.lookup (block_id, tid) track_signals of
                Just tsig | wants_tsig flags t ->
                    BlockC.set_track_signal view_id tracknum tsig
                _ -> return ()
        _ -> return ()
    where
    set_style_low = update_set_style state block_id tlike_id set_style
    merged = events_of_track_ids state (Block.dtrack_merged dtrack)

-- | Convert SetStyleHigh to lower level SetStyle by giving it information not
-- available at lowel levels.  For the moment that's just an ad-hoc
-- 'has_note_children' flag.  This is a bit awkward and ad-hoc, but the
-- alternative is setting some flag in the 'Ui.Track.Track', and that's just
-- one more thing that can get out of sync.
update_set_style :: State.State -> BlockId -> Block.TracklikeId
    -> Track.SetStyleHigh -> Track.SetStyle
update_set_style state block_id (Block.TId track_id _) (track_bg, set_style) =
    (track_bg, set_style note_children)
    where
    note_children = either (const False) id $ State.eval state $
        has_note_children block_id track_id
update_set_style _ _ _ (track_bg, set_style) = (track_bg, set_style False)

has_note_children :: State.M m => BlockId -> TrackId -> m Bool
has_note_children block_id track_id = do
    children <- fromMaybe [] <$> TrackTree.children_of block_id track_id
    return $ any (ParseTitle.is_note_track . State.track_title) children

merged_events_of :: State.State -> Block.Block -> TrackNum -> [Events.Events]
merged_events_of state block tracknum =
    case Seq.at (Block.block_tracks block) tracknum of
        Just track -> events_of_track_ids state (Block.track_merged track)
        Nothing -> []

-- | Don't send a track signal to a track unless it actually wants to draw it.
wants_tsig :: Set.Set Block.TrackFlag -> Track.Track -> Bool
wants_tsig flags track =
    Track.render_style (Track.track_render track) /= Track.NoRender
    && Block.Collapse `Set.notMember` flags

-- | Generate the title for block windows.
--
-- This is @block - view@, where @view@ will have @block@ stripped from the
-- beginning, e.g. @b1 - b1.v1@ becomes @b1 - .v1@.
block_window_title :: Id.Namespace -> ViewId -> BlockId -> Text
block_window_title ns view_id block_id = block <> " - " <> strip block view
    where
    block = txt $ Id.show_short ns (Id.unpack_id block_id)
    view = txt $ Id.show_short ns (Id.unpack_id view_id)
    strip prefix txt = fromMaybe txt $ Text.stripPrefix prefix txt

events_of_track_ids :: State.State -> [TrackId] -> [Events.Events]
events_of_track_ids state track_ids = mapMaybe events_of track_ids
    where
    events_of track_id = fmap Track.track_events (Map.lookup track_id tracks)
    tracks = State.state_tracks state

-- | Convert Types.Selection to BlockC.Selection.  Return sets of tracknums and
-- the selections they should have.
track_selections :: Types.SelNum -> TrackNum -> Maybe Types.Selection
    -> [([TrackNum], [BlockC.Selection])]
track_selections selnum tracks maybe_sel = case maybe_sel of
    Nothing -> [([0 .. tracks - 1], [])]
    Just sel -> (clear, []) : convert_selection selnum tracks sel
        where
        (low, high) = Types.sel_track_range sel
        clear = [0 .. low - 1] ++ [high + 1 .. tracks - 1]

convert_selection :: Types.SelNum -> TrackNum -> Types.Selection
    -> [([TrackNum], [BlockC.Selection])]
convert_selection selnum tracks sel = filter (not . null . fst) $
    ([cur_track | Num.in_range 0 tracks cur_track], [selection True])
    : [(tracknums, [selection False]) | not (null tracknums)]
    where
    tracknums = filter (/=cur_track) (Types.sel_tracknums tracks sel)
    cur_track = Types.sel_cur_track sel
    selection arrow = BlockC.Selection
        { BlockC.sel_color = color
        , BlockC.sel_start = Types.sel_start_pos sel
        , BlockC.sel_cur = Types.sel_cur_pos sel
        , BlockC.sel_draw_arrow = arrow
        }
    color = Config.lookup_selection_color selnum

dtracks_with_ruler_id :: State.M m =>
    RulerId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
dtracks_with_ruler_id ruler_id =
    find_dtracks ((== Just ruler_id) . Block.ruler_id_of)
        <$> State.gets State.state_blocks

dtracks_with_track_id :: State.M m =>
    TrackId -> m [(BlockId, [(TrackNum, Block.TracklikeId)])]
dtracks_with_track_id track_id =
    find_dtracks ((== Just track_id) . Block.track_id_of)
        <$> State.gets State.state_blocks

find_dtracks :: (Block.TracklikeId -> Bool) -> Map.Map BlockId Block.Block
    -> [(BlockId, [(TrackNum, Block.TracklikeId)])]
find_dtracks f blocks = do
    (bid, b) <- Map.assocs blocks
    let tracks = get_tracks b
    guard (not (null tracks))
    return (bid, tracks)
    where
    all_tracks block = Seq.enumerate (Block.block_display_tracks block)
    get_tracks block =
        [ (tracknum, Block.dtracklike_id track)
        | (tracknum, track) <- all_tracks block, f (Block.dtracklike_id track)
        ]
