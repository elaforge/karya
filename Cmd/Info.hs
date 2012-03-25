{- | Functions to get higher level information about blocks and tracks.

    This builds on "Derive.TrackInfo" but the Derive module only has functions
    needed by derivation, and doesn't run in the State monad.
-}
module Cmd.Info where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Text.Printf as Printf

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Tree

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import Types


-- * track info

data Track = Track {
    track_info :: State.TrackInfo
    , track_type :: TrackType
    } deriving (Show, Eq)

data TrackType =
    -- | The pitch track for this note track, if any.  This is the last pitch
    -- child, or the first pitch parent.  Any intervening note tracks stop the
    -- search, because the pitch track is considered to belong to them.
    Note (Maybe State.TrackInfo)
    -- | The note track for this pitch track.
    | Pitch (Maybe State.TrackInfo)
    -- | Tracks this control track has scope over.  This means all its
    -- children, but because of inversion, also its parents until the next
    -- note track, if any.
    | Control [State.TrackInfo]
    deriving (Show, Eq)

get_track_type :: (State.M m) => BlockId -> TrackNum -> m Track
get_track_type block_id tracknum = State.require
    ("get_track_type: bad tracknum: " ++ show (block_id, tracknum))
        =<< lookup_track_type block_id tracknum

lookup_track_type :: (State.M m) => BlockId -> TrackNum -> m (Maybe Track)
lookup_track_type block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    return $ make_track <$> paths_of track_tree tracknum

-- | Get all the Tracks in a block, sorted by tracknum.
block_tracks :: (State.M m) => BlockId -> m [Track]
block_tracks block_id = Seq.sort_on (State.track_tracknum . track_info)
    . map make_track . Util.Tree.paths <$> State.get_track_tree block_id

make_track :: (State.TrackInfo, [State.TrackInfo], [State.TrackInfo]) -> Track
make_track (track, parents, children) =
    Track track (track_type_of track parents children)

track_type_of :: State.TrackInfo -> [State.TrackInfo] -> [State.TrackInfo]
    -> TrackType
track_type_of track parents children
    | TrackInfo.is_note_track title =
        Note $ List.find is_pitch (reverse (takeWhile (not.is_note) children))
            `mplus` List.find is_pitch (takeWhile (not.is_note) parents)
    | TrackInfo.is_pitch_track title =
        Pitch $ List.find is_note (children ++ parents)
    | otherwise = Control $ children ++ takeWhile (not.is_note) parents
    where
    title = State.track_title track
    is_pitch = TrackInfo.is_pitch_track . State.track_title
    is_note = TrackInfo.is_note_track . State.track_title

-- ** specialized lookups

-- | Pitch track of a note track, if any.
pitch_of_note :: (State.M m) => BlockId -> TrackNum -> m (Maybe State.TrackInfo)
pitch_of_note block_id tracknum = do
    maybe_track <- lookup_track_type block_id tracknum
    return $ case maybe_track of
        Just (Track _ (Note pitch)) -> pitch
        _ -> Nothing

-- | Note track of a pitch track, if any.
note_of_pitch :: (State.M m) => BlockId -> TrackNum -> m (Maybe State.TrackInfo)
note_of_pitch block_id tracknum = do
    maybe_track <- lookup_track_type block_id tracknum
    return $ case maybe_track of
        Just (Track _ (Pitch note)) -> note
        _ -> Nothing


-- * misc

-- | Get the instrument of a track, or fail.
get_instrument_of :: (State.M m) => BlockId -> TrackNum -> m Score.Instrument
get_instrument_of block_id tracknum = do
    track <- State.get_track
        =<< State.get_event_track_at "get_instrument_of" block_id tracknum
    State.require ("get_instrument_of expected a note track: "
            ++ show (block_id, tracknum)) $
        TrackInfo.title_to_instrument (Track.track_title track)


-- * inst info

inst_info :: (Cmd.M m) => Score.Instrument -> m String
inst_info inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    alloc <- State.get_midi_alloc
    let show_info = show_instrument_info (Map.findWithDefault [] inst alloc)
    return $ show_inst inst ++ ": " ++ maybe "<not found>" show_info maybe_info

show_instrument_info :: [Instrument.Addr] -> Cmd.MidiInfo -> String
show_instrument_info addrs info = unlines
    [ "keyswitches: " ++ show_keyswitch_map
        (Instrument.patch_keyswitches (MidiDb.info_patch info))
    , "addrs: " ++ show_addrs addrs
    ]

-- | Looks like: "wdev1 [0..2]; wdev2 [0,4]"
show_addrs :: [Instrument.Addr] -> String
show_addrs addrs = show_list2
    [ Pretty.pretty wdev ++ " "
        ++ "[" ++ Seq.join "," (show_runs (map snd addrs)) ++ "]"
    | (wdev, addrs) <- Seq.keyed_group_on fst addrs]

show_inst :: Score.Instrument -> String
show_inst (Score.Instrument name) = '>' : name

show_keyswitch_map :: Instrument.KeyswitchMap -> String
show_keyswitch_map (Instrument.KeyswitchMap attr_ks) = show_list $
    map (('+':) . Seq.join "+" . Set.elems . Score.attrs_set . fst) attr_ks

show_list, show_list2 :: [String] -> String
show_list [] = "[]"
show_list xs = Seq.join ", " xs

show_list2 [] = "[]"
show_list2 xs = Seq.join "; " xs

str :: String -> String
str "" = '"' : '"' : ""
str s = s

show_runs :: (Show a, Num a, Ord a) => [a] -> [String]
show_runs = concatMap show_run . Seq.split_between (\a b -> a+1 < b)
    where
    show_run xs@(_:_:_:_) = [show (head xs) ++ ".." ++ show (last xs)]
    show_run xs = map show xs

-- * set_inst_status

-- | Stick some handy info about the current instrument into the status.
--
-- This should be run whenever the track focus changes, or tracks are expanded
-- or collapsed.
set_inst_status :: (Cmd.M m) => BlockId -> TrackNum -> m ()
set_inst_status block_id tracknum = do
    ttree <- State.get_track_tree block_id
    -- This may be run loading a new state when there is no focused block, so
    -- be careful to not abort the cmd.
    maybe_block_id <- Cmd.lookup_focused_block
    case maybe_block_id of
        Just block_id -> do
            status <- get_track_status block_id ttree tracknum
            Cmd.set_global_status "inst" status
        Nothing -> return ()

-- | Looks like:
-- title (tracknum): inst_name, allocation, [control tracks]
-- fm8/inst1 at 1: fm8:0,1,2, [vel {collapse 2}, pedal {expand 3}]
get_track_status :: (Cmd.M m) => BlockId -> State.TrackTree -> TrackNum
    -> m String
get_track_status block_id ttree tracknum = case note_track_of ttree tracknum of
    Just (inst, note_tracknum) -> do
        let controls = control_tracks_of ttree note_tracknum
        track_descs <- show_track_status block_id controls
        addrs <- Map.findWithDefault [] inst <$> State.get_midi_alloc
        let title = TrackInfo.instrument_to_title inst
        return $ Printf.printf "%s at %d: %s -- [%s]" title note_tracknum
            (show_addrs addrs) (Seq.join ", " track_descs)
    Nothing -> return $ "track " ++ show tracknum ++ ": no inst"

find_track :: TrackNum -> State.TrackTree -> Maybe (Tree.Tree State.TrackInfo)
find_track tracknum = Util.Tree.find ((==tracknum) . State.track_tracknum)

control_tracks_of :: State.TrackTree -> TrackNum -> [State.TrackInfo]
control_tracks_of ttree tracknum = case paths_of ttree tracknum of
        Nothing -> []
        Just (_, parents, _) -> controls parents
    where
    controls = filter (is_control . State.track_title)
    is_control title = not (TrackInfo.is_tempo_track title)
    -- TODO a "control of" should be defined as the "single parents", every
    -- parent that has only one child

-- | Given a tracknum, find the note track below it.  Since there may
-- be multiple ones, pick the first one.
--
-- Search down the children for a instrument title, then get the first note
-- track (tree bottom) underneath that.
--
-- This assumes the instrument is specified in a track below, not inherited
-- from the environment.  Inheriting the instrument seems hard to do, but may
-- be possible.  Maybe I can sove this problem later.
note_track_of :: State.TrackTree -> TrackNum
    -> Maybe (Score.Instrument, TrackNum)
note_track_of ttree tracknum = case find_track tracknum ttree of
        Nothing -> Nothing
        Just tree -> find_inst tree
    where
    find_inst tree@(Tree.Node track children) = case inst_of track of
        Nothing -> msum (map find_inst children)
        Just inst -> Just
            (inst, State.track_tracknum (Util.Tree.first_leaf tree))
    inst_of = TrackInfo.title_to_instrument . State.track_title

-- | Looks like: [vel {collapse 2}, pedal {expand 3}]
show_track_status :: (State.M m) => BlockId -> [State.TrackInfo] -> m [String]
show_track_status block_id status = forM status $ \info -> do
    let tracknum = State.track_tracknum info
    btrack <- State.block_track_at block_id tracknum
    let cmd_text = case fmap Block.track_flags btrack of
            Nothing -> "?"
            Just flags
                | Block.Collapse `elem` flags -> "expand"
                | otherwise -> "collapse"
    return $ Printf.printf "%s {%s %d}"
        (str (TrackInfo.strip_expr (State.track_title info))) cmd_text tracknum

paths_of :: State.TrackTree -> TrackNum
    -> Maybe (State.TrackInfo, [State.TrackInfo], [State.TrackInfo])
paths_of track_tree tracknum =
    List.find ((==tracknum) . State.track_tracknum . (\(a, _, _) -> a))
        (Util.Tree.paths track_tree)
