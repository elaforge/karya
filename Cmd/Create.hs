{- | Cmds to create and destroy blocks, views, tracks, and rulers.
-}
module Cmd.Create where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.MakeRuler as MakeRuler

import qualified App.Config as Config


-- * block

-- | BlockIds look like \"proj.b0\", \"proj.b1\", etc.
block :: (State.UiStateMonad m) => Ruler.RulerId -> m Block.BlockId
block ruler_id = do
    project <- State.get_project
    blocks <- fmap State.state_blocks State.get
    block_id <- require "block id" $ generate_block_id project blocks
    b <- State.create_block block_id $
        Block.block "" Config.block_config [] Config.schema
    State.insert_track b 0 (Block.RId ruler_id) Config.ruler_width
    return b


generate_block_id project blocks =
    generate_id project "" "b" Block.BlockId blocks


-- * view

view :: (State.UiStateMonad m) => Block.BlockId -> m Block.ViewId
view block_id = do
    views <- State.get_views_of block_id
    view_id <- require "view id" $ generate_view_id views block_id
    rect <- fmap (find_rect Config.view_size . map Block.view_rect . Map.elems
        . State.state_views) State.get
    State.create_view view_id $ Block.view block_id rect Config.view_config

block_view :: (State.UiStateMonad m) => Ruler.RulerId -> m Block.ViewId
block_view ruler_id = block ruler_id >>= view

-- | ViewIds look like \"proj.b0.v0\", \"proj.b0.v1\", etc.
generate_view_id views block_id =
    -- project is "" since it should be inherited from the block id.
    generate_id "" (Block.un_block_id block_id) "v" Block.ViewId views

-- | Same as State.destroy_view, included here for consistency.
destroy_view :: (State.UiStateMonad m) => Block.ViewId -> m ()
destroy_view view_id = State.destroy_view view_id

-- * track

-- | Tracks look like \"proj.b0.t0\", etc.
track_ruler :: (State.UiStateMonad m) =>
    Block.BlockId -> Ruler.RulerId -> Block.TrackNum -> Block.Width
    -> m Track.TrackId
track_ruler block_id ruler_id tracknum width = do
    tracks <- State.get_tracks_of block_id
    track_id <- require "track id" $
        generate_track_id block_id "t" tracks
    tid <- State.create_track track_id (empty_track "")
    State.insert_track block_id tracknum (Block.TId tid ruler_id) width
    return tid

-- | Like 'track_ruler', but copy the ruler and track width from the track to
-- the left.
track :: (State.UiStateMonad m) =>
    Block.BlockId -> Block.TrackNum -> m Track.TrackId
track block_id tracknum = do
    -- Clip to valid range to callers can use an out of range tracknum.
    tracknum <- clip_tracknum block_id tracknum
    maybe_track <- State.track_at block_id (tracknum-1)
    let (ruler_id, width) = case maybe_track of
            Just ((Block.TId _ rid), width) -> (rid, width)
            Just ((Block.RId rid), width) ->
                let s = Ruler.un_ruler_id rid
                    rid2 = if overlay_suffix `List.isSuffixOf` s then rid
                        else Ruler.RulerId (s ++ overlay_suffix)
                in (rid2, width)
            _ -> (State.no_ruler, Config.track_width)
    -- The above can generate a bad ruler_id if they didn't use 'ruler' to
    -- create the ruler with the overlay version, so abort early if that's the
    -- case.
    State.get_ruler ruler_id
    track_ruler block_id ruler_id tracknum width

clip_tracknum block_id tracknum = do
    tracks <- State.tracks block_id
    return $ max 0 (min tracks tracknum)

-- | Controller tracks are created relative to another track, and look like
-- \"proj.b0.t0_velocity\".
controller_track :: (State.UiStateMonad m) =>
    Block.BlockId -> Ruler.RulerId -> Block.TrackNum -> String
    -> m Track.TrackId
controller_track block_id ruler_id controlled_tracknum cont = do
    controlled <- State.track_at block_id controlled_tracknum
    controlled_track_id <- require ("tracknum " ++ show controlled_tracknum) $
        (fmap fst controlled) >>= tracklike_track

    let controlled_track_name = last $ Seq.split "."
            (Track.un_track_id controlled_track_id)
        track_name = controlled_track_name ++ "_" ++ cont
    named_track block_id ruler_id (controlled_tracknum+1) track_name cont

-- | Create a track with the given name and title.
-- Looks like \"proj.b0.tempo\".
named_track :: (State.UiStateMonad m) =>
    Block.BlockId -> Ruler.RulerId -> Block.TrackNum
    -> String -> String -> m Track.TrackId
named_track block_id ruler_id tracknum name title = do
    let track_id = Seq.join "." [Block.un_block_id block_id, name]
    all_tracks <- fmap State.state_tracks State.get
    when (Track.TrackId track_id `Map.member` all_tracks) $
        State.throw $ "track " ++ show track_id ++ " already exists"
    tid <- State.create_track track_id (empty_track title)
    State.insert_track block_id tracknum
        (Block.TId tid ruler_id) Config.track_width
    return tid

-- ** cmds

append_track :: (Monad m) => Cmd.CmdT m Track.TrackId
append_track = do
    block_id <- Cmd.get_focused_block
    track block_id 99999

insert_track_after_selection :: (Monad m) => Cmd.CmdT m Track.TrackId
insert_track_after_selection = do
    (_, tracknum, _) <- Selection.get_insert_pos
    block_id <- Cmd.get_focused_block
    track block_id (tracknum+1)

remove_selected_tracks :: (Monad m) => Cmd.CmdT m ()
remove_selected_tracks = do
    block_id <- Cmd.get_focused_block
    sel <- fmap snd $ Selection.selected_tracks Config.insert_selnum
    mapM_ (State.remove_track block_id) (reverse
        [ Block.sel_start_track sel
        .. Block.sel_start_track sel + (Block.sel_tracks sel - 1)])

-- ** util

empty_track title = Track.track title [] Config.track_bg

tracklike_track (Block.TId tid _) = Just tid
tracklike_track _ = Nothing

generate_track_id block_id code tracks =
    generate_id "" (Block.un_block_id block_id) code Track.TrackId tracks


-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swap_tracks :: (State.UiStateMonad m) => Block.BlockId
    -> Block.TrackNum -> Block.TrackNum -> m ()
swap_tracks block_id num0 num1 = do
    track0 <- State.track_at block_id num0
    track1 <- State.track_at block_id num1
    case (track0, track1) of
        (Nothing, Nothing) -> return ()
        (Just (t0, w0), Nothing) -> remove num0 >> insert num1 t0 w0
        (Nothing, Just (t1, w1)) -> remove num1 >> insert num0 t1 w1
        (Just (t0, w0), Just (t1, w1)) -> do
            remove num0 >> insert num0 t1 w1
            remove num1 >> insert num1 t0 w0
    where
    remove = State.remove_track block_id
    insert = State.insert_track block_id

-- * ruler

-- | This creates both a ruler with the given name, and an overlay version
-- named with .overlay.
ruler :: (State.UiStateMonad m) => [Ruler.NameMarklist] -> String
    -> m (Ruler.RulerId, Ruler.RulerId)
ruler marklists name = do
    let ruler = MakeRuler.ruler marklists
    rid <- State.create_ruler name ruler
    over_rid <- State.create_ruler
        (name ++ overlay_suffix) (MakeRuler.as_overlay ruler)
    return (rid, over_rid)

-- | An overlay versions of a ruler has id ruler_id ++ suffix.
overlay_suffix :: String
overlay_suffix = ".overlay"

-- * util

generate_id project parent code typ fm =
    List.find (not . (`Map.member` fm) . typ) candidates
    where candidates = ids_for project parent code

ids_for :: String -> String -> String -> [String]
ids_for project parent code =
    [dot project ++ dot parent ++ code ++ show n | n <- [0..]]
    where dot s = if null s then "" else s ++ "."

require msg = maybe (State.throw $ "somehow can't find ID for " ++ msg) return

-- TODO I also need the screen dimensions to do this right.  Before I go
-- too far here, though, I'll want to think about proper window manager stuff.
-- If I just allow the placement function to be passed as an arg...
find_rect (w, h) rects = Block.Rect (right, bottom) (w, h)
    where
    right = maximum $ 0 : map Block.rect_right rects
    bottom = 10
