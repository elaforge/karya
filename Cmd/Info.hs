-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Functions to get higher level information about blocks and tracks.

    This builds on "Derive.ParseTitle" but the Derive module only has functions
    needed by derivation, and doesn't run in the State monad.
-}
module Cmd.Info where
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Text.Printf as Printf

import qualified Util.Seq as Seq
import qualified Util.Tree as Tree
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Perform.Midi.Patch as Patch
import Global
import Types


-- * track info

data Track = Track {
    track_info :: State.TrackInfo
    , track_type :: TrackType
    } deriving (Show, Eq)

data TrackType =
    -- | A note track has a list of control tracks, and a list of child note
    -- tracks.  The controls start with the children and continues with its
    -- parents.  However, it stops at another note track or as soon as a parent
    -- has more than one child, because that control track doesn't belong to
    -- just this note track.
    Note [State.TrackInfo] [State.TrackInfo]
    -- | The note track for this pitch track.
    | Pitch (Maybe State.TrackInfo)
    -- | Tracks this control track has scope over.  This means all its
    -- children, but because of inversion, also a parent note track, if there
    -- is one.
    | Control [State.TrackInfo]
    deriving (Show, Eq)

get_track_type :: State.M m => BlockId -> TrackNum -> m Track
get_track_type block_id tracknum = State.require
    ("get_track_type: bad tracknum: " <> showt (block_id, tracknum))
        =<< lookup_track_type block_id tracknum

lookup_track_type :: State.M m => BlockId -> TrackNum -> m (Maybe Track)
lookup_track_type block_id tracknum = do
    track_tree <- TrackTree.track_tree_of block_id
    return $ make_track <$>
        Tree.find_with_parents ((==tracknum) . State.track_tracknum) track_tree

-- | Get all the Tracks in a block, sorted by tracknum.
block_tracks :: State.M m => BlockId -> m [Track]
block_tracks block_id = Seq.sort_on (State.track_tracknum . track_info)
    . map make_track . Tree.paths <$> TrackTree.track_tree_of block_id

make_track :: (Tree.Tree State.TrackInfo, TrackTree.TrackTree) -> Track
make_track (tree, parents) =
    Track (Tree.rootLabel tree) (track_type_of (tree, parents))

track_type_of :: (Tree.Tree State.TrackInfo, TrackTree.TrackTree) -> TrackType
track_type_of (Tree.Node track subs, parents)
    | ParseTitle.is_note_track title = Note
        (takeWhile is_control children ++ takeWhile is_control
            (map Tree.rootLabel (takeWhile is_single parents)))
        (filter is_note children)
    | ParseTitle.is_pitch_track title =
        Pitch $ List.find is_note (children ++ map Tree.rootLabel parents)
    | otherwise = Control $ children
        -- If there is a note track above assume it will invert itself below
        -- the control stack.
        ++ maybe [] (:[]) (List.find is_note (map Tree.rootLabel parents))
    where
    children = concatMap Tree.flatten subs
    is_single (Tree.Node _ [_]) = True
    is_single _ = False
    is_control track =
        ParseTitle.is_control_track t && not (ParseTitle.is_tempo_track t)
        where t = State.track_title track
    title = State.track_title track
    is_note = ParseTitle.is_note_track . State.track_title

-- ** specialized lookups

-- | Pitch track of a note track, if any.
pitch_of_note :: State.M m => BlockId -> TrackNum -> m (Maybe State.TrackInfo)
pitch_of_note block_id tracknum = do
    maybe_track <- lookup_track_type block_id tracknum
    return $ case maybe_track of
        Just (Track _ (Note controls _)) ->
            List.find (ParseTitle.is_pitch_track . State.track_title) controls
        _ -> Nothing

-- | Note track of a pitch track, if any.
note_of_pitch :: State.M m => BlockId -> TrackNum
    -> m (Maybe State.TrackInfo)
note_of_pitch block_id tracknum = do
    maybe_track <- lookup_track_type block_id tracknum
    return $ case maybe_track of
        Just (Track _ (Pitch note)) -> note
        _ -> Nothing


-- * misc

-- | Get the instrument of a track, or fail if it's not a note track.  This is
-- different than 'Perf.lookup_instrument' because it looks at the track title
-- first.  This is useful for new tracks which don't have a performance yet.
-- But if the track title doesn't specify an instrument it falls back on
-- 'Perf.lookup_instrument'.
get_instrument_of :: Cmd.M m => BlockId -> TrackNum -> m Score.Instrument
get_instrument_of block_id tracknum =
    State.require ("get_instrument_of expected a note track: "
            <> showt (block_id, tracknum))
        =<< lookup_instrument_of block_id tracknum

lookup_instrument_of :: Cmd.M m => BlockId -> TrackNum
    -> m (Maybe Score.Instrument)
lookup_instrument_of block_id tracknum = do
    track_id <- State.get_event_track_at block_id tracknum
    track <- State.get_track track_id
    case ParseTitle.title_to_instrument (Track.track_title track) of
        Nothing -> Perf.lookup_instrument (block_id, Just track_id)
        Just inst -> Just <$> get_default_instrument block_id track_id inst

-- | If the instrument is 'Score.empty_instrument', look up what it really is
-- in the performance.
get_default_instrument :: Cmd.M m => BlockId -> TrackId
    -> Score.Instrument -> m Score.Instrument
get_default_instrument block_id track_id inst
    | inst == Score.empty_instrument =
        fromMaybe inst <$> Perf.lookup_instrument (block_id, Just track_id)
    | otherwise = return inst


-- * inst info

-- | Looks like: "wdev1 [0..2]; wdev2 [0,4]"
show_addrs :: [Patch.Addr] -> Text
show_addrs addrs = semicolon_list
    [ pretty wdev
        <> " [" <> Text.intercalate "," (show_runs (map snd addrs)) <> "]"
    | (wdev, addrs) <- Seq.keyed_group_sort fst addrs
    ]

comma_list, semicolon_list :: [Text] -> Text
comma_list [] = "[]"
comma_list xs = Text.intercalate ", " xs

semicolon_list [] = "[]"
semicolon_list xs = Text.intercalate "; " xs

show_runs :: (Show a, Num a, Ord a) => [a] -> [Text]
show_runs = concatMap show_run . Seq.split_between (\a b -> a+1 < b)
    where
    show_run xs@(_:_:_:_) = [showt (head xs) <> ".." <> showt (last xs)]
    show_run xs = map showt xs


-- * set_instrument_status

-- | Stick some handy info about the current instrument into the status.
--
-- This should be run whenever the track focus changes, or tracks are expanded
-- or collapsed.
set_instrument_status :: Cmd.M m => BlockId -> TrackNum -> m ()
set_instrument_status block_id tracknum = do
    status <- get_track_status block_id tracknum
    whenJust status $ Cmd.set_global_status "inst"

-- | Looks like:
-- title (tracknum): inst_name, allocation, [control tracks]
-- fm8/inst1 at 1: fm8:0,1,2, [vel {collapse 2}, pedal {expand 3}]
get_track_status :: Cmd.M m => BlockId -> TrackNum -> m (Maybe Text)
get_track_status block_id tracknum = do
    tree <- TrackTree.track_tree_of block_id
    case find_note_track tree tracknum of
        Just (track, inst) -> do
            inst <- get_default_instrument block_id (State.track_id track) inst
            Just <$> status block_id tree (State.track_tracknum track) inst
        Nothing -> return Nothing
    where
    status block_id tree note_tracknum inst = do
        let controls = control_tracks_of tree note_tracknum
        track_descs <- show_track_status block_id controls
        alloc <- State.allocation inst <#> State.get
        let addrs = case StateConfig.alloc_backend <$> alloc of
                Just (StateConfig.Midi config) -> Patch.config_addrs config
                _ -> []
        let title = ParseTitle.instrument_to_title inst
        return $ txt $ Printf.printf "%s at %d: %s -- [%s]" (untxt title)
            note_tracknum (untxt (show_addrs addrs))
            (Seq.join ", " track_descs)

-- | Given a tracknum, find the note track associated with it.  Since there
-- may be multiple ones, pick the first one.  First try children, then
-- parents.
find_note_track :: TrackTree.TrackTree -> TrackNum
    -> Maybe (State.TrackInfo, Score.Instrument)
find_note_track tree tracknum = case paths_of tree tracknum of
        Nothing -> Nothing
        Just (track, parents, children) ->
            Seq.head $ mapMaybe inst_of (track : children ++ parents)
    where
    inst_of track =
        case ParseTitle.title_to_instrument (State.track_title track) of
            Nothing -> Nothing
            Just inst -> Just (track, inst)

-- | Get the controls associated with the given track.  This means all
-- children until the next note track, and all parents with only one child
-- until the next note track.  Parents with multiple children are not
-- associated with a single track, so they're omitted.  Tempo tracks are always
-- omitted.
control_tracks_of :: TrackTree.TrackTree -> TrackNum -> [State.TrackInfo]
control_tracks_of tree tracknum =
    case Tree.find_with_parents ((==tracknum) . State.track_tracknum) tree of
        Nothing -> []
        Just (Tree.Node _ children, parents) ->
            takeWhile is_control (concatMap Tree.flatten children)
            ++ takeWhile is_control (map Tree.rootLabel
                (takeWhile is_single parents))
    where
    is_single (Tree.Node _ [_]) = True
    is_single _ = False
    is_control track =
        ParseTitle.is_control_track t && not (ParseTitle.is_tempo_track t)
        where t = State.track_title track

-- | Looks like: [vel {collapse 2}, pedal {expand 3}]
show_track_status :: State.M m => BlockId -> [State.TrackInfo] -> m [String]
show_track_status block_id status = forM status $ \info -> do
    let tracknum = State.track_tracknum info
    btrack <- State.block_track_at block_id tracknum
    let cmd_text :: String
        cmd_text = case fmap Block.track_flags btrack of
            Nothing -> "?"
            Just flags
                | Block.Collapse `Set.member` flags -> "expand"
                | otherwise -> "collapse"
    return $ Printf.printf "%s {%s %d}"
        (untxt $ ParseTitle.strip_expr $ State.track_title info)
        cmd_text tracknum

paths_of :: TrackTree.TrackTree -> TrackNum
    -> Maybe (State.TrackInfo, [State.TrackInfo], [State.TrackInfo])
    -- ^ (track, parents, children)
paths_of track_tree tracknum =
    List.find ((==tracknum) . State.track_tracknum . (\(a, _, _) -> a))
        (Tree.flat_paths track_tree)

