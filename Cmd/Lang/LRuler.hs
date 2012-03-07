module Cmd.Lang.LRuler where
import qualified Data.Map as Map
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import Types


-- | Replace the meter for the rulers of this block.
set_meter :: MakeRuler.Meter -> BlockId -> Cmd.CmdL ()
set_meter meter block_id = do
    dur <- State.block_event_end block_id
    when (dur <= 0) $
        Cmd.throw $ "can't set ruler for block with 0 duration"
    replace_marklist_in block_id (MakeRuler.fit_ruler dur meter)


{- Examples:
Create.ruler [MakeRuler.meter_ruler 16 MakeRuler.m44] "meter_44"

replace_marklist (rid "r1") "meter" (MakeRuler.meter_ruler 16 MakeRuler.m44)
copy_marklist "meter" (rid "r1") (rid "r1.overlay")

block >>= LRuler.set_meter $ MakeRuler.repeat 2 $
  MakeRuler.regular_subdivision [3, 5, 4, 4]
block >>= LRuler.set_meter $ MakeRuler.regular_subdivision [4, 2, 3, 4]

Make a new ruler:

assign_new "new-ruler" (map bid [...])

-}

show_ruler :: RulerId -> Cmd.CmdL String
show_ruler ruler_id = do
    Ruler.Ruler mlists bg show_names use_alpha align_to_bottom full_width
        <- State.get_ruler ruler_id
    return $ PPrint.record
        [ ("bg", show bg)
        , ("show_names", show show_names), ("use_alpha", show use_alpha)
        , ("full_width", show full_width)
        , ("align_to_bottom", show align_to_bottom)
        , ("marklists", PPrint.list (Map.keys mlists))
        ]

show_marklist :: RulerId -> Ruler.Name -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist marklist_name ruler_id
    return $ PPrint.list $
        map (\(pos, m) -> Printf.printf "%s - %s" (show pos) (Pretty.pretty m))
            (Ruler.ascending mlist 0)

get_marklist :: (Cmd.M m) => Ruler.Name -> RulerId -> m Ruler.Marklist
get_marklist name ruler_id = do
    ruler <- State.get_ruler ruler_id
    case Ruler.get_marklist name ruler of
        Nothing -> Cmd.throw $
            "no marklist " ++ show name ++ " in " ++ show ruler_id
        Just mlist -> return mlist

-- | Replace or add a marklist with the given name.
replace_marklist :: (Cmd.M m) => RulerId -> (Ruler.Name, Ruler.Marklist)
    -> m ()
replace_marklist ruler_id (name, mlist) =
    State.modify_ruler ruler_id (Ruler.set_marklist name mlist)

-- | Modify just the given marklist.
modify_marklist :: (Cmd.M m) => RulerId -> Ruler.Name
    -> (Ruler.Marklist -> Ruler.Marklist) -> m ()
modify_marklist ruler_id name modify = State.modify_ruler ruler_id $
    Ruler.modify_marklist name modify

replace_marklist_in :: (Cmd.M m) => BlockId -> (Ruler.Name, Ruler.Marklist)
    -> m ()
replace_marklist_in block_id mlist =
    mapM_ (flip replace_marklist mlist) =<< State.rulers_of block_id

modify_rulers :: (Cmd.M m) => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_rulers block_id modify =
    mapM_ (flip State.modify_ruler modify) =<< State.rulers_of block_id

modify_ruler :: (Cmd.M m) => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_ruler block_id modify = do
    ruler_id <- State.ruler_of block_id
    State.modify_ruler ruler_id modify

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copy_marklist :: Ruler.Name -> RulerId -> RulerId -> Cmd.CmdL ()
copy_marklist marklist_name from_ruler_id to_ruler_id = do
    mlist <- get_marklist marklist_name from_ruler_id
    replace_marklist to_ruler_id (marklist_name, mlist)

-- | Make a new ruler that's a copy of the ruler of the first block, and then
-- assign that ruler to all the blocks.
assign_new :: String -> [BlockId] -> Cmd.CmdL ()
assign_new name block_ids = do
    ruler <- State.get_ruler =<< State.ruler_of
        =<< Cmd.require (Seq.head block_ids)
    (ruler_id, overlay_id) <- Create.ruler name ruler
    mapM_ (Create.set_block_ruler ruler_id overlay_id) block_ids

-- | Replace the rulers in the block with the given ruler_id.  If there is an
-- overlay version, it will be given to all but the first track.
replace :: RulerId -> BlockId -> Cmd.CmdL ()
replace ruler_id block_id = do
    _ <- State.get_ruler ruler_id -- Just make sure it exists.
    let overlay_id = Create.add_overlay_suffix ruler_id
    overlay_id <- fmap (maybe ruler_id (const overlay_id)) $
        State.lookup_ruler overlay_id
    State.modify_block block_id $ \block -> block
        { Block.block_tracks = map_head_tail
            (set_r ruler_id) (set_r overlay_id) (Block.block_tracks block)
        }
    where
    map_head_tail _ _ [] = []
    map_head_tail f g (x:xs) = f x : map g xs
    set_r ruler_id track = Block.modify_id track (Block.set_rid ruler_id)

-- | Drop a mark at the selected point in the "cue" ruler.
add_cue :: String -> Cmd.CmdL ()
add_cue text = do
    (block_id, _, _, pos) <- Selection.get_insert
    add_cue_at block_id pos text

add_cue_at :: BlockId -> ScoreTime -> String -> Cmd.CmdL ()
add_cue_at block_id pos text = modify_ruler block_id $
    Ruler.modify_marklist "cue" $ Ruler.insert_mark pos (cue_mark text)

cue_mark :: String -> Ruler.Mark
cue_mark text = Ruler.Mark 0 2 Color.black text 0 0

-- * extract

extract :: Cmd.CmdL ()
extract = do
    (block_id, _, track_id, _) <- Selection.get_insert
    extract_from block_id track_id

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
extract_from :: (Cmd.M m) => BlockId -> TrackId -> m ()
extract_from block_id track_id = do
    subs <- extract_subs track_id
    ruler_ids <- mapM State.ruler_of [bid | (_, _, bid) <- subs]
    mlists <- mapM (get_marklist MakeRuler.meter_marklist) ruler_ids
    let big_mlist = Ruler.place_marklists
            [(start, dur, mlist) | ((start, dur, _), mlist) <- zip subs mlists]
    replace_marklist_in block_id
        (MakeRuler.meter_marklist, MakeRuler.recalculate_zoom big_mlist)

extract_subs :: (Cmd.M m) => TrackId -> m [(ScoreTime, ScoreTime, BlockId)]
extract_subs track_id = do
    events <- Events.ascending . Track.track_events <$>
        State.get_track track_id
    ns <- State.get_namespace
    let call = NoteTrack.block_call ns . Event.event_string
    return $ do
        (pos, evt) <- events
        Just block_id <- return (call evt)
        return (pos, Event.event_duration evt, block_id)
