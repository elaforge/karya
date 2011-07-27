module Cmd.Lang.LRuler where
import Control.Monad
import qualified Data.List as List
import qualified Text.Printf as Printf

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler


-- | Replace the meter for the rulers of this block.
set_meter :: MakeRuler.Meter -> BlockId -> Cmd.CmdL ()
set_meter meter block_id = do
    dur <- State.block_event_end block_id
    when (dur <= 0) $
        Cmd.throw $ "can't set ruler for block with 0 duration"
    let mlist = MakeRuler.fit_ruler dur meter
    ruler_ids <- rulers_of block_id
    mapM_ (flip replace_marklist mlist) ruler_ids

rulers_of :: BlockId -> Cmd.CmdL [RulerId]
rulers_of block_id =
    Seq.unique . Block.block_ruler_ids <$> State.get_block block_id


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
        , ("marklists", PPrint.list (map fst mlists))
        ]

show_marklist :: RulerId -> Ruler.MarklistName -> Cmd.CmdL String
show_marklist ruler_id marklist_name = do
    mlist <- get_marklist ruler_id marklist_name
    return $ PPrint.list $
        map (\(pos, m) -> Printf.printf "%s - %s" (show pos) (Pretty.pretty m))
            (Ruler.forward mlist 0)

get_marklist :: RulerId -> Ruler.MarklistName -> Cmd.CmdL Ruler.Marklist
get_marklist ruler_id marklist_name = do
    ruler <- State.get_ruler ruler_id
    case lookup marklist_name (Ruler.ruler_marklists ruler) of
        Nothing -> Cmd.throw $
            "no marklist " ++ show marklist_name ++ " in " ++ show ruler_id
        Just mlist -> return mlist

replace_marklist :: RulerId -> Ruler.NameMarklist -> Cmd.CmdL ()
replace_marklist ruler_id (name, mlist) = do
    ruler <- State.get_ruler ruler_id
    i <- case List.findIndex ((==name) . fst) (Ruler.ruler_marklists ruler) of
        Nothing -> return 0
        Just i -> State.remove_marklist ruler_id i >> return i
    State.insert_marklist ruler_id i (name, mlist)

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copy_marklist :: Ruler.MarklistName -> RulerId -> RulerId -> Cmd.CmdL ()
copy_marklist marklist_name from_ruler_id to_ruler_id = do
    mlist <- get_marklist from_ruler_id marklist_name
    replace_marklist to_ruler_id (marklist_name, mlist)

-- | Make a new ruler that's a copy of the ruler of the first block, and then
-- assign that ruler to all the blocks.
assign_new :: String -> [BlockId] -> Cmd.CmdL ()
assign_new name block_ids = do
    ruler <- State.get_ruler =<< ruler_of
        =<< Cmd.require (Seq.head block_ids)
    (ruler_id, overlay_id) <- Create.ruler name ruler
    mapM_ (set_block ruler_id overlay_id) block_ids

set_block :: RulerId -> RulerId -> BlockId -> Cmd.CmdL ()
set_block ruler_id overlay_id block_id = modify_tracks block_id set
    where
    set (Block.TId tid _) = Block.TId tid overlay_id
    set (Block.RId _) = Block.RId ruler_id
    set t = t

modify_tracks :: BlockId -> (Block.TracklikeId -> Block.TracklikeId)
    -> Cmd.CmdL ()
modify_tracks block_id f = State.modify_block block_id $ \block ->
    block { Block.block_tracks = map modify (Block.block_tracks block) }
    where modify t = t { Block.tracklike_id = f (Block.tracklike_id t) }

ruler_of :: BlockId -> Cmd.CmdL RulerId
ruler_of block_id = Cmd.require
    =<< Seq.head <$> Block.block_ruler_ids <$> State.get_block block_id

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
