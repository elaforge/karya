-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Infrastructure for dealing with rulers in a higher level way.
module Cmd.Ruler.RulerUtil where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Lists as Lists
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * generate

-- | Make a ruler of a single meter until the given end time.
meterUntil :: Meter.AbstractMeter -> TrackTime -> Int -> TrackTime
    -> Meter.Meter
meterUntil meter measureDur perSection end =
    -- TODO why meter_take if I'm already setting the right number of sections?
    meterTake end $ Meter.meter Meter.defaultConfig $
    replicate sections (Meter.MSection perSection measureDur meter)
        ++ [Meter.MSection left measureDur meter]
    where
    (sections, left) = measures `divMod` perSection
    measures = ceiling (end / measureDur)

-- * by marklist

-- | Copy a marklist from one ruler to another.  If it already exists in
-- the destination ruler, it will be replaced.
copyMarklist :: Ui.M m => Ruler.Name -> RulerId -> RulerId -> m ()
copyMarklist name fromRulerId toRulerId = do
    fromRuler <- Ui.getRuler fromRulerId
    let mlist = Ruler.getMarklist name fromRuler
    setMarklist toRulerId name mlist

-- | Replace or add a marklist with the given name.
setMarklist :: Ui.M m => RulerId -> Ruler.Name -> Mark.Marklist -> m ()
setMarklist rulerId name mlist =
    Ui.modifyRuler rulerId (Right . Ruler.setMarklist name mlist)

-- * by Meter

getMeter :: Ui.M m => RulerId -> m Meter.Meter
getMeter = fmap Ruler.getMeter . Ui.getRuler

setMeter :: Ui.M m => RulerId -> Meter.Meter -> m ()
setMeter rulerId meter = Ui.modifyRuler rulerId $
    Right . Ruler.setMeter meter

meterTake :: TrackTime -> Meter.Meter -> Meter.Meter
meterTake = Meter.modifySections . Meter.sectionsTake

meterDrop :: TrackTime -> Meter.Meter -> Meter.Meter
meterDrop = Meter.modifySections . Meter.sectionsDrop

extract :: TrackTime -> TrackTime -> [Meter.MSection] -> [Meter.MSection]
extract start end = Meter.sectionsDrop start . Meter.sectionsTake end

delete :: TrackTime -> TrackTime -> [Meter.MSection] -> [Meter.MSection]
delete start end ss = Meter.sectionsTake start ss <> Meter.sectionsDrop end ss

-- * ModifyRuler

type ModifyRuler = Ruler.Ruler -> Either Text Ruler.Ruler

-- | The scope of a ruler modification.
data Scope
    -- | Modify all rulers on the block.
    = Block
    -- | Modify the 'section_ruler_id'.  Section 0 is the 'Ui.block_ruler'.
    | Section !TrackNum
    -- | Modify the given tracks.
    | Tracks ![TrackNum]
    deriving (Eq, Show)

-- | Create or replace a ruler for the given block.
replace :: Ui.M m => BlockId -> ModifyRuler -> m RulerId
replace blockId modify = do
    whenM (Maybe.isNothing <$> Ui.lookupRuler rulerId) $
        void $ Ui.createRuler (Id.unpackId rulerId) Ruler.empty
    Ui.modifyRuler rulerId modify
    pure rulerId
    where
    rulerId = Id.RulerId $ Id.unpackId blockId

-- ** local

-- | Modify a ruler or rulers, making copies if they're shared outside of
-- the Scope.
local :: Ui.M m => Scope -> BlockId -> ModifyRuler -> m [RulerId]
local scope blockId modify = case scope of
    Block -> localBlock blockId modify
    Section tracknum -> (:[]) <$> localSection blockId tracknum modify
    Tracks tracknums -> localTracks blockId tracknums modify

-- | Like 'local', but specialized to modify the meter.
localMeter :: Ui.M m => Scope -> BlockId -> (Meter.Meter -> Meter.Meter)
    -> m [RulerId]
localMeter scope blockId modify =
    local scope blockId (Right . Ruler.modifyMeter modify)

localBlock :: Ui.M m => BlockId -> ModifyRuler -> m [RulerId]
localBlock blockId modify =
    mapM (\rulerId -> localRuler Block blockId rulerId modify)
        =<< Ui.rulersOf blockId

localSection :: Ui.M m => BlockId -> TrackNum
    -- ^ Modify the 'section_ruler_id' of this track.
    -> ModifyRuler -> m RulerId
localSection blockId tracknum modify = do
    rulerId <- sectionRulerId blockId tracknum
    localRuler (Section tracknum) blockId rulerId modify

-- | Modify the given tracks, making copies if they're shared with other
-- tracks.
localTracks :: Ui.M m => BlockId -> [TrackNum] -> ModifyRuler -> m [RulerId]
localTracks blockId tracknums modify = do
    rulerIds <- rulersOf blockId tracknums
    forM rulerIds $ \rulerId ->
        localRuler (Tracks tracknums) blockId rulerId modify

-- | Modify the given RulerId, making a copy if it's shared with another block.
localRuler :: Ui.M m => Scope -> BlockId -> RulerId -> ModifyRuler
    -> m RulerId
localRuler scope blockId rulerId modify = do
    (inScope, outScope) <- rulersInScope scope blockId rulerId
    if rulerId /= Ui.noRuler && not outScope
        then do
            Ui.modifyRuler rulerId modify
            pure rulerId
        else do
            newRulerId <- modifyCopy blockId rulerId modify
            forM_ inScope $ \tracknum ->
                Ui.setTrackRuler blockId tracknum newRulerId
            pure newRulerId

rulersInScope :: Ui.M m => Scope -> BlockId -> RulerId
    -> m ([TrackNum], Bool) -- ^ (tracknums with the RulerId and in the scope,
    -- True if there are ones out of scope)
rulersInScope scope blockId rulerId = do
    blocks <- Ui.blocksWithRulerId rulerId
    let inBlock :: [TrackNum]
        (inBlock, outBlock) = (bimap (map fst . concatMap snd) (not . null)) $
            List.partition ((==blockId) . fst) blocks
    case scope of
        Block -> pure (inBlock, outBlock)
        Section tracknum -> do
            section <- map fst <$> getSection blockId tracknum
            let (inSection, outSection) = List.partition
                    (`elem` section) inBlock
            pure (inSection, outBlock || not (null outSection))
        Tracks tracknums ->
            pure (inTrack, outBlock || not (null outTrack))
            where
            (inTrack, outTrack) = List.partition (`elem` tracknums) inBlock

-- | Copy the ruler and modify the copy.
modifyCopy :: Ui.M m => BlockId -> RulerId -> ModifyRuler -> m RulerId
modifyCopy blockId rulerId modify = do
    -- Try to the ruler the same name as the block, since it's now
    -- local to that block, appending numbers if the name is taken.
    -- This can happen if this block is a copy of another with a local
    -- RulerId.
    newRulerId <- blockIdToFreeRuler <$>
        Ui.gets Ui.stateRulers <*> pure blockId
    newRulerId <- copy newRulerId rulerId
    Ui.modifyRuler newRulerId modify
    pure newRulerId

-- ** modify

-- | Modify the ruler on the focused block.  Other blocks with the same ruler
-- will also be modified.
modify :: Ui.M m => Scope -> BlockId -> ModifyRuler -> m ()
modify scope blockId modify = case scope of
    Block -> modifyBlock blockId modify
    Section tracknum -> modifySection blockId tracknum modify
    Tracks tracknums -> modifyTracks blockId tracknums modify

-- | Modify all rulers in the block.
modifyBlock :: Ui.M m => BlockId -> ModifyRuler -> m ()
modifyBlock blockId modify = do
    tracks <- Ui.trackCount blockId
    mapM_ (\rulerId -> modifyRuler blockId [0 .. tracks-1] rulerId modify)
        =<< Ui.rulersOf blockId

-- | Modify all rulers on the block in this 'section_ruler_id'.
modifySection :: Ui.M m => BlockId -> TrackNum -> ModifyRuler -> m ()
modifySection blockId tracknum modify = do
    rulerId <- sectionRulerId blockId tracknum
    tracknums <- if rulerId == Ui.noRuler
        then map fst <$> getSection blockId tracknum
        else pure [tracknum]
    modifyRuler blockId tracknums rulerId modify

modifyTracks :: Ui.M m => BlockId -> [TrackNum] -> ModifyRuler -> m ()
modifyTracks blockId tracknums modify = do
    rulerIds <- rulersOf blockId tracknums
    forM_ rulerIds $ \rulerId ->
        modifyRuler blockId tracknums rulerId modify

modifyRuler :: Ui.M m => BlockId -> [TrackNum]
    -- ^ If given, these are the TrackIds that were intended to be modified.
    -- This is necessary if you try to modify Ui.no_ruler.
    -> RulerId -> ModifyRuler -> m ()
modifyRuler blockId tracknums rulerId modify
    | rulerId == Ui.noRuler = do
        newRulerId <- modifyCopy blockId rulerId modify
        forM_ tracknums $ \tracknum ->
            Ui.setTrackRuler blockId tracknum newRulerId
    | otherwise = Ui.modifyRuler rulerId modify

-- ** util

-- | The section RulerId is the first ruler of the section.  A section is
-- defined by 'get_section', but in a normal block with one ruler track at
-- tracknum 0, it will be tracknum 0.
sectionRulerId :: Ui.M m => BlockId -> TrackNum -> m RulerId
sectionRulerId blockId tracknum =
    Ui.require ("no rulers in " <> pretty (blockId, tracknum))
            . Lists.head . Block.rulerIdsOf . map snd
        =<< getSection blockId tracknum

-- | Get the tracks of the section of the given track.  A section extends
-- from a ruler track until the next ruler track.
getSection :: Ui.M m => BlockId -> TrackNum
    -> m [(TrackNum, Block.TracklikeId)]
getSection blockId tracknum = do
    tracks <- map snd <$> blockTracks blockId
    let sections = Lists.splitBefore (Maybe.isJust . rulerIdOf . snd)
            (zip [0..] tracks)
    pure $ fromMaybe [] $ List.find ((tracknum `elem`) . map fst) sections
    where
    rulerIdOf (Block.RId rid) = Just rid
    rulerIdOf _ = Nothing

-- | Create a block-local RulerId.  It gets the exact same name as the block,
-- but is defined as a function so it's clear where this conversion is done.
blockIdToRuler :: BlockId -> Id.Id
blockIdToRuler = Id.unpackId

blockIdToFreeRuler :: Map RulerId a -> BlockId -> Id.Id
blockIdToFreeRuler rulers blockId = ident
    where
    -- Always just since the list is infinite.
    Just ident = List.find (not . (`Map.member` rulers) . Id.RulerId) $
        map (Id.id ns) $ name : [name <> "-" <> showt n | n <- [1..]]
    (ns, name) = Id.unId $ Id.unpackId blockId

copy :: Ui.M m => Id.Id -> RulerId -> m RulerId
copy ident rulerId = Ui.createRuler ident =<< Ui.getRuler rulerId

rulersOf :: Ui.M m => BlockId -> [TrackNum] -> m [RulerId]
rulersOf blockId tracknums = Lists.unique . Block.rulerIdsOf . map snd
    . filter ((`elem` tracknums) . fst) <$> blockTracks blockId

blockTracks :: Ui.M m => BlockId -> m [(TrackNum, Block.TracklikeId)]
blockTracks = fmap (zip [0..] . map Block.tracklikeId . Block.blockTracks)
    . Ui.getBlock
