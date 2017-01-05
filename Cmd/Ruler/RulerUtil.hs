-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Infrastructure for dealing with rulers in a higher level way than as
-- a 'Ruler.Marklist'.
module Cmd.Ruler.RulerUtil where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import qualified Cmd.Ruler.Meter as Meter
import Global
import Types


type ModifyRuler = Ruler.Ruler -> Either Text Ruler.Ruler

-- * constructors

-- | Create a ruler with a meter of the given duration.
meter_ruler :: Meter.MeterConfig -> Meter.Duration -> [Meter.AbstractMeter]
    -> Ruler.Ruler
meter_ruler config dur meters = Ruler.meter_ruler (Just Meter.mtype) $
    Meter.meter_marklist config (Meter.fit_meter dur meters)

-- | Replace or add a marklist with the given name.
set_marklist :: Ui.M m => RulerId -> Ruler.Name -> Maybe Ruler.MeterType
    -> Ruler.Marklist -> m ()
set_marklist ruler_id name mtype mlist =
    Ui.modify_ruler ruler_id (Right . Ruler.set_marklist name mtype mlist)

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copy_marklist :: Ui.M m => Ruler.Name -> RulerId -> RulerId -> m ()
copy_marklist name from_ruler_id to_ruler_id = do
    from_ruler <- Ui.get_ruler from_ruler_id
    let (mtype, mlist) = Ruler.get_marklist name from_ruler
    set_marklist to_ruler_id name mtype mlist

-- * meter

get_meter :: Ui.M m => RulerId -> m Meter.LabeledMeter
get_meter = fmap Meter.ruler_meter . Ui.get_ruler

-- | The scope of a ruler modification.
data Scope =
    -- | Modify all rulers on the block.
    Block
    -- | Modify the 'section_ruler_id'.  Section 0 is the 'Ui.block_ruler'.
    | Section !TrackNum
    -- | Modify the given tracks.
    | Tracks ![TrackNum]
    deriving (Eq, Show)

-- ** local

-- | Modify a ruler or rulers, making copies if they're shared outside of
-- the Scope.
local :: Ui.M m => Scope -> BlockId -> ModifyRuler -> m [RulerId]
local scope block_id modify = case scope of
    Block -> local_block block_id modify
    Section tracknum -> (:[]) <$> local_section block_id tracknum modify
    Tracks tracknums -> local_tracks block_id tracknums modify

local_block :: Ui.M m => BlockId -> ModifyRuler -> m [RulerId]
local_block block_id modify =
    mapM (\ruler_id -> local_ruler Block block_id ruler_id modify)
        =<< Ui.rulers_of block_id

local_section :: Ui.M m => BlockId -> TrackNum
    -- ^ Modify the 'section_ruler_id' of this track.
    -> ModifyRuler -> m RulerId
local_section block_id tracknum modify = do
    ruler_id <- section_ruler_id block_id tracknum
    local_ruler (Section tracknum) block_id ruler_id modify

-- | Modify the given tracks, making copies if they're shared with other
-- tracks.
local_tracks :: Ui.M m => BlockId -> [TrackNum] -> ModifyRuler -> m [RulerId]
local_tracks block_id tracknums modify = do
    ruler_ids <- rulers_of block_id tracknums
    forM ruler_ids $ \ruler_id ->
        local_ruler (Tracks tracknums) block_id ruler_id modify

-- | Modify the given RulerId, making a copy if it's shared with another block.
local_ruler :: Ui.M m => Scope -> BlockId -> RulerId -> ModifyRuler
    -> m RulerId
local_ruler scope block_id ruler_id modify = do
    (in_scope, out_scope) <- rulers_in_scope scope block_id ruler_id
    if ruler_id /= Ui.no_ruler && not out_scope
        then do
            Ui.modify_ruler ruler_id modify
            return ruler_id
        else do
            new_ruler_id <- modify_copy block_id ruler_id modify
            forM_ in_scope $ \tracknum ->
                Ui.set_track_ruler block_id tracknum new_ruler_id
            return new_ruler_id

rulers_in_scope :: Ui.M m => Scope -> BlockId -> RulerId
    -> m ([TrackNum], Bool) -- ^ (tracknums with the RulerId and in the scope,
    -- True if there are ones out of scope)
rulers_in_scope scope block_id ruler_id = do
    blocks <- Ui.blocks_with_ruler_id ruler_id
    let in_block :: [TrackNum]
        (in_block, out_block) = ((map fst . concatMap snd) *** (not . null)) $
            List.partition ((==block_id) . fst) blocks
    case scope of
        Block -> return (in_block, out_block)
        Section tracknum -> do
            section <- map fst <$> get_section block_id tracknum
            let (in_section, out_section) = List.partition
                    (`elem` section) in_block
            return (in_section, out_block || not (null out_section))
        Tracks tracknums ->
            return (in_track, out_block || not (null out_track))
            where
            (in_track, out_track) = List.partition (`elem` tracknums) in_block

-- | Copy the ruler and modify the copy.
modify_copy :: Ui.M m => BlockId -> RulerId -> ModifyRuler -> m RulerId
modify_copy block_id ruler_id modify = do
    -- Try to the ruler the same name as the block, since it's now
    -- local to that block, appending numbers if the name is taken.
    -- This can happen if this block is a copy of another with a local
    -- RulerId.
    new_ruler_id <- block_id_to_free_ruler <$>
        Ui.gets Ui.state_rulers <*> return block_id
    new_ruler_id <- copy new_ruler_id ruler_id
    Ui.modify_ruler new_ruler_id modify
    return new_ruler_id

-- ** modify

-- | Modify the ruler on the focused block.  Other blocks with the same ruler
-- will also be modified.
modify :: Ui.M m => Scope -> BlockId -> ModifyRuler -> m ()
modify scope block_id modify = case scope of
    Block -> modify_block block_id modify
    Section tracknum -> modify_section block_id tracknum modify
    Tracks tracknums -> modify_tracks block_id tracknums modify

-- | Modify all rulers in the block.
modify_block :: Ui.M m => BlockId -> ModifyRuler -> m ()
modify_block block_id modify = do
    tracks <- Ui.track_count block_id
    mapM_ (\ruler_id -> modify_ruler block_id [0 .. tracks-1] ruler_id modify)
        =<< Ui.rulers_of block_id

-- | Modify all rulers on the block in this 'section_ruler_id'.
modify_section :: Ui.M m => BlockId -> TrackNum -> ModifyRuler -> m ()
modify_section block_id tracknum modify = do
    ruler_id <- section_ruler_id block_id tracknum
    tracknums <- if ruler_id == Ui.no_ruler
        then map fst <$> get_section block_id tracknum
        else return [tracknum]
    modify_ruler block_id tracknums ruler_id modify

modify_tracks :: Ui.M m => BlockId -> [TrackNum] -> ModifyRuler -> m ()
modify_tracks block_id tracknums modify = do
    ruler_ids <- rulers_of block_id tracknums
    forM_ ruler_ids $ \ruler_id ->
        modify_ruler block_id tracknums ruler_id modify

modify_ruler :: Ui.M m => BlockId -> [TrackNum]
    -- ^ If given, these are the TrackIds that were intended to be modified.
    -- This is necessary if you try to modify Ui.no_ruler.
    -> RulerId -> ModifyRuler -> m ()
modify_ruler block_id tracknums ruler_id modify
    | ruler_id == Ui.no_ruler = do
        new_ruler_id <- modify_copy block_id ruler_id modify
        forM_ tracknums $ \tracknum ->
            Ui.set_track_ruler block_id tracknum new_ruler_id
    | otherwise = Ui.modify_ruler ruler_id modify

-- ** util

-- | The section RulerId is the first ruler of the section.  A section is
-- defined by 'get_section', but in a normal block with one ruler track at
-- tracknum 0, it will be tracknum 0.
section_ruler_id :: Ui.M m => BlockId -> TrackNum -> m RulerId
section_ruler_id block_id tracknum =
    Ui.require ("no rulers in " <> pretty (block_id, tracknum))
            . Seq.head . Block.ruler_ids_of . map snd
        =<< get_section block_id tracknum

-- | Get the tracks of the section of the given track.  A section extends
-- from a ruler track until the next ruler track.
get_section :: Ui.M m => BlockId -> TrackNum
    -> m [(TrackNum, Block.TracklikeId)]
get_section block_id tracknum = do
    tracks <- map snd <$> block_tracks block_id
    let sections = Seq.split_with (Maybe.isJust . ruler_id_of . snd)
            (zip [0..] tracks)
    return $ fromMaybe [] $ List.find ((tracknum `elem`) . map fst) sections
    where
    ruler_id_of (Block.RId rid) = Just rid
    ruler_id_of _ = Nothing

-- | Create a block-local RulerId.  It gets the exact same name as the block,
-- but is defined as a function so it's clear where this conversion is done.
block_id_to_ruler :: BlockId -> Id.Id
block_id_to_ruler = Id.unpack_id

block_id_to_free_ruler :: Map.Map RulerId a -> BlockId -> Id.Id
block_id_to_free_ruler rulers block_id = ident
    where
    -- Always just since the list is infinite.
    Just ident = List.find (not . (`Map.member` rulers) . Id.RulerId) $
        map (Id.id ns) $ name : [name <> "-" <> showt n | n <- [1..]]
    (ns, name) = Id.un_id $ Id.unpack_id block_id

copy :: Ui.M m => Id.Id -> RulerId -> m RulerId
copy ident ruler_id = Ui.create_ruler ident =<< Ui.get_ruler ruler_id

rulers_of :: Ui.M m => BlockId -> [TrackNum] -> m [RulerId]
rulers_of block_id tracknums = Seq.unique . Block.ruler_ids_of . map snd
    . filter ((`elem` tracknums) . fst) <$> block_tracks block_id

block_tracks :: Ui.M m => BlockId -> m [(TrackNum, Block.TracklikeId)]
block_tracks = fmap (zip [0..] . map Block.tracklike_id . Block.block_tracks)
    . Ui.get_block
