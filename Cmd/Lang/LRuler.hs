module Cmd.Lang.LRuler where
import qualified Data.Map as Map
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import Types



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

meter :: Ruler.Name
meter = "meter"

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

-- * general purpose

-- | Double the 'meter' marklist in the current block. You can then trim it
-- down to size.
double :: Cmd.CmdL ()
double = do
    block_id <- Cmd.get_focused_block
    modify_block block_id $ Ruler.modify_marklist meter $ \mlist ->
        MakeRuler.renumber_marklist $
            mlist <> Ruler.shift (Ruler.marklist_end mlist) mlist

-- | Clip the ruler to the selection.
clip :: Cmd.CmdL ()
clip = do
    (block_id, _, _, pos) <- Selection.get_insert
    modify_block block_id $ Ruler.clip pos

-- | Copy the ruler under the selection and append it.
--
-- Extract 'meter' under each ruler.  Then make a modifier that appends it to
-- each ruler, and pass to 'modify' for each ruler id.
append :: Cmd.CmdL ()
append = do
    (view_id, sel) <- Selection.get
    block_id <- State.block_id_of view_id
    let (start, end) = Types.sel_range sel
    modify_block block_id (append_range MakeRuler.meter_marklist start end)

append_range :: Ruler.Name -> ScoreTime -> ScoreTime -> Ruler.Ruler
    -> Ruler.Ruler
append_range name start end = Ruler.modify_marklist name $ \mlist ->
        mlist <> Ruler.marklist (shift (extract mlist))
    where
    extract mlist = takeWhile ((<end) . fst) $ Ruler.ascending mlist start
    shift = map (first (+ (end-start)))

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
        event <- events
        Just block_id <- return (call event)
        return (Event.start event, Event.duration event, block_id)

-- * individual marklists

show_marklist :: RulerId -> Ruler.Name -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist marklist_name ruler_id
    return $ PPrint.list $
        map (\(pos, m) -> Printf.printf "%s - %s" (show pos) (Pretty.pretty m))
            (Ruler.ascending mlist 0)

get_marklist :: (Cmd.M m) => Ruler.Name -> RulerId -> m Ruler.Marklist
get_marklist name ruler_id = do
    maybe (Cmd.throw $ "no marklist " ++ show name ++ " in " ++ show ruler_id)
        return =<< lookup_marklist name ruler_id

lookup_marklist :: (Cmd.M m) => Ruler.Name -> RulerId
    -> m (Maybe Ruler.Marklist)
lookup_marklist name ruler_id =
    Ruler.lookup_marklist name <$> State.get_ruler ruler_id

-- | Replace the meter for the rulers of this block.
set_meter :: MakeRuler.Meter -> BlockId -> Cmd.CmdL ()
set_meter meter block_id = do
    dur <- State.block_event_end block_id
    when (dur <= 0) $
        Cmd.throw $ "can't set ruler for block with 0 duration"
    replace_marklist_in block_id (MakeRuler.fit_ruler dur meter)

-- | Replace or add a marklist with the given name.
replace_marklist :: (Cmd.M m) => RulerId -> (Ruler.Name, Ruler.Marklist)
    -> m ()
replace_marklist ruler_id (name, mlist) =
    State.modify_ruler ruler_id (Ruler.set_marklist name mlist)

-- * cue

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

-- * util

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
    let overlay_id = Create.add_overlay_suffix ruler_id
    overlay_id <- fmap (maybe ruler_id (const overlay_id)) $
        State.lookup_ruler overlay_id
    State.set_track_ruler block_id 0 ruler_id
    count <- State.track_count block_id
    forM_ [1..count-1] $ \tracknum ->
        State.set_track_ruler block_id tracknum overlay_id

-- | Modify all the rulers on a block.
modify_block :: (State.M m) => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m ()
modify_block block_id f = do
    ruler_ids <- State.rulers_of block_id
    mapM_ (modify f block_id) ruler_ids

-- | Modify the given RulerId, making a new one if it's already in use on
-- a block other than the give one.
--
-- TODO if there's another ruler with the same content, share them
modify :: (State.M m) => (Ruler.Ruler -> Ruler.Ruler) -> BlockId -> RulerId
    -> m RulerId
modify modify block_id ruler_id = do
    blocks <- State.tracks_with_ruler_id ruler_id
    case blocks of
        [(rblock_id, _)] | block_id == rblock_id -> do
            State.modify_ruler ruler_id modify
            return ruler_id
        _ -> do
            new_ruler_id <- copy (ruler_id_for_block block_id ruler_id) ruler_id
            State.modify_ruler new_ruler_id modify
            sequence_ $ do
                (rblock_id, tracks) <- blocks
                guard (rblock_id == block_id)
                (tracknum, _) <- tracks
                return $ State.set_track_ruler block_id tracknum new_ruler_id
            return new_ruler_id

-- | Make a RulerId with the same name as the BlockId.  But I should preserve
-- .overlay.  So, use the ruler_id but prepend the block name.
--
-- TODO Id should have a concat_id so I don't have to use unsafe_id
ruler_id_for_block :: BlockId -> RulerId -> Id.Id
ruler_id_for_block block_id ruler_id =
    Id.unsafe_id ns (block_name ++ "." ++ ruler_name)
    where
    block_name = Id.ident_name block_id
    (ns, ruler_name) = Id.un_id (Id.unpack_id ruler_id)

copy :: (State.M m) => Id.Id -> RulerId -> m RulerId
copy ident ruler_id = State.create_ruler ident =<< State.get_ruler ruler_id
