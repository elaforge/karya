-- | Cmds that display some info for the user.
--
-- These will be frequently be called from the language, but may also be used
-- by built in cmds.  Since they are intended for human consumption, many
-- of them return strings.
module Cmd.Info where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Text.Printf as Printf

import qualified Util.Seq as Seq
import qualified Util.Tree

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


inst_info :: (Cmd.M m) => Score.Instrument -> m String
inst_info inst = do
    maybe_info <- Cmd.lookup_instrument_info inst
    midi_config <- State.get_midi_config
    let show_info = show_instrument_info
            (Map.findWithDefault [] inst (Instrument.config_alloc midi_config))
    return $ show_inst inst ++ ": " ++ maybe "<not found>" show_info maybe_info

show_instrument_info :: [Instrument.Addr] -> MidiDb.Info -> String
show_instrument_info addrs info = unlines
    [ "keyswitches: " ++ show_keyswitch_map
        (Instrument.patch_keyswitches (MidiDb.info_patch info))
    , "addrs: " ++ show_addrs addrs
    ]

-- | Looks like: "wdev1 [0..2]; wdev2 [0,4]"
show_addrs :: [Instrument.Addr] -> String
show_addrs addrs = show_list2
    [ Midi.un_write_device wdev ++ " "
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

show_runs :: (Num a, Ord a) => [a] -> [String]
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
        midi_config <- State.get_midi_config
        let addrs = Map.findWithDefault [] inst
                (Instrument.config_alloc midi_config)
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
show_track_status :: (State.UiStateMonad m) => BlockId -> [State.TrackInfo]
    -> m [String]
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
