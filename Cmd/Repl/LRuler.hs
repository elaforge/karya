-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Work with rulers and meters.  A meter is a marklist on a ruler named
    'Ruler.meter', and is used by "Cmd.TimeStep" to align things.  By
    convention the meter has regular subdivisions, with 'Ruler.Rank's that
    correspond roughly to timestep durations (e.g. whole, half, quarter notes).
    The ruler marks are numbered, and this module, with the support of
    "Cmd.RulerUtil", lets you modify the meter in a higher level way and takes
    care of renumbering the labels so e.g. measure numbers always count up even
    if you double the length of the meter.

    Ultimately this is necessary because I want to manipulate rulers as a high
    level 'Meter.Meter' or 'Meter.LabeledMeter', but 'Ruler.Marklist' has lost
    the meter's structure.  That in turn is because different kinds of meters,
    talams, gong cycles, etc. have different structures and I didn't think
    I could come up with a single high level data structure that fit them all
    and still allowed generic manipulation.

    Many functions emit a 'Modify'.  If defaults to 'RulerUtil.Section' scope,
    but you can change it to work on selected tracks with 'tracks' or all
    rulers in the block with 'block'.  Then, the 'modify' function will
    destructively modify selected rulers, while the 'local' function will
    modify via copy-on-write, so that other blocks or tracks are unaffected.

    Examples:

    - Start at a different measure number:

        > LRuler.modify $ LRuler.set_start_measure 4

    - Bali: 8 gongs with 4 jegogans per gong.  Since counts are on calung, and
    there are 2 calung per jegogan, this is basically an 8 beat cycle:

        > LRuler.modify $ LRuler.gongs 8 4

    - Give the current block 6 sections of standard 4/4 meter, with 4 measures
    per section, where each measure gets 1t:

        > LRuler.modify $ LRuler.measures Meters.m44 6 4

    - TODO make a middle measure 5/4?

    - Set a block to 8 avartanams of adi talam:

        > LRuler.local $ LRuler.ruler $ Tala.adi 8

    - Change the selected tracks to 8 avartanams of tisram:

        > LRuler.local $ LRuler.tracks $ LRuler.ruler $
        >   Tala.simple Tala.adi_tala 3 8

    - Slow and fast rupaka, chatusra nadai:

        > LRuler.local $ LRuler.ruler $ Tala.simple Tala.rupaka_tala 4 8
        > LRuler.local $ LRuler.ruler $ Tala.simple Tala.rupaka_fast 4 8

    - Set a block to 8 avartanams of adi talam, then select tracks and set them
    to chatusram-tisram:

        > LRuler.modify $ LRuler.ruler $ Tala.adi 8
        > LRuler.local $ LRuler.tracks $ LRuler.ruler $ LTala.chatis 8 4
-}
module Cmd.Repl.LRuler where
import           Prelude hiding (concat)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Ruler.Extract as Extract
import qualified Cmd.Ruler.Gong as Gong
import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Cmd.Selection as Selection

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ruler as Ruler
import qualified Ui.Ui as Ui

import           Global
import           Types


-- * general purpose

rename :: RulerId -> RulerId -> Cmd.CmdL ()
rename = Create.renameRuler

-- | List all rulers, along with the number of blocks each one appears in.
listn :: Cmd.CmdL [(RulerId, Int)]
listn = map (second length) <$> list

list :: Ui.M m => m [(RulerId, [BlockId])]
list = do
    rulerIds <- Ui.allRulerIds
    blockIds <- mapM Ui.blocksWithRulerId rulerIds
    pure $ zip rulerIds (map (map fst) blockIds)

-- | Destroy all unrefereced rulers, and return their now-invalid RulerIds.
gc :: Ui.M m => m [RulerId]
gc = do
    rulerIds <- Create.orphanRulers
    mapM_ Ui.destroyRuler rulerIds
    pure rulerIds

-- | Group together rulers that are the same, replace all the duplicates with
-- the first ruler in each group, then gc away the duplicates.  Return the
-- duplicates.
unify :: Ui.M m => m [[RulerId]]
unify = do
    groups <- Lists.groupStable snd <$>
        Ui.gets (Map.toAscList . Ui.stateRulers)
    mapM_ merge groups
    gc
    pure $ filter ((>1) . length) $ map (map fst . NonEmpty.toList) groups
    where
    merge ((rid, _) :| dups) = forM_ (map fst dups) $ \dupRid ->
        replaceRulerId dupRid rid

-- | After copying blocks around and fiddling with rulers, the RulerIds can
-- wind up with names from other blocks.  Synchronize RulerIds along with their
-- owning BlockIds.  A RulerId only on one BlockId is assumed to be local to
-- that block, and will get its name.
syncIds :: Ui.M m => m Text
syncIds = do
    deleted <- unify
    let unified = if null deleted then "" else Text.unlines $
            "Unified:" : [pretty x <> " <- " <> pretty xs | x : xs <- deleted]
            ++ [""]
    misnamed <- listMisnamed
    let renames = [(rulerId, RulerUtil.blockIdToRuler blockId)
            | (rulerId, blockId) <- misnamed]
    Create.renameRulers renames
    let renamed = if null renames then "" else Text.unlines $
            "Renamed:" : [ pretty from <> " -> "
                <> pretty (Id.RulerId to) | (from, to) <- renames]
    pure $ unified <> renamed

listMisnamed :: Ui.M m => m [(RulerId, BlockId)]
listMisnamed = go <$> list
    where
    go rulerBlocks =
        [ (rulerId, blockId)
        | (rulerId, Just blockId) <- map (second len1) rulerBlocks
        , RulerUtil.blockIdToRuler blockId /= Id.unpackId rulerId
        ]
    len1 [x] = Just x
    len1 _ = Nothing

-- | Blocks that contain the given ruler.
blocksOf :: Ui.M m => RulerId -> m [BlockId]
blocksOf = fmap (map fst) . Ui.blocksWithRulerId

-- | Set the rulers on a block to the given RulerId.
setRulerId :: Ui.M m => RulerId -> BlockId -> m ()
setRulerId rulerId blockId = do
    old <- Ui.blockRuler blockId
    Ui.replaceRulerId blockId old rulerId

-- | Copy the ruler of the given block to the current one.
copy :: Cmd.M m => BlockId -> m ()
copy otherBlock = do
    otherRuler <- Ui.rulerOf otherBlock
    thisBlock <- Cmd.getFocusedBlock
    thisRuler <- Ui.blockRuler thisBlock
    Ui.replaceRulerId thisBlock thisRuler otherRuler

-- | Set the ruler of the tracks in the given scope.
set :: Ui.M m => RulerId -> BlockId -> RulerUtil.Scope -> m ()
set rulerId blockId scope = do
    rulerIds <- case scope of
        RulerUtil.Block -> do
            count <- Ui.trackCount blockId
            pure $ replicate count (Just rulerId)
        RulerUtil.Section tracknum -> replaceTracknums
            =<< map fst <$> RulerUtil.getSection blockId tracknum
        RulerUtil.Tracks tracknums -> replaceTracknums tracknums
    Ui.setRulerIds blockId rulerIds
    where
    replaceTracknums tracknums = do
        old <- map Block.rulerIdOf . Block.blockTracklikeIds <$>
                Ui.getBlock blockId
        let replace tracknum oldRuler
                | tracknum `elem` tracknums = Just rulerId
                | otherwise = oldRuler
        pure $ zipWith replace [0..] old

-- | Replace the ruler.
ruler :: Cmd.M m => Meter.Meter -> m Modify
ruler meter = do
    (blockId, tracknum) <- getBlockTrack
    pure $ makeModify blockId tracknum $ const $ Right $
        Ruler.meterRuler meter

lruler :: Cmd.M m => Meter.Meter -> m [RulerId]
lruler = local . ruler

-- | Modify all rulers.
modifyRulers :: Cmd.M m => (Ruler.Ruler -> Ruler.Ruler) -> m ()
modifyRulers modify = do
    rulerIds <- Ui.allRulerIds
    forM_ rulerIds $ \rulerId ->
        Ui.modifyRuler rulerId (Right . modify)

-- | Replace all occurrences of one RulerId with another.
replaceRulerId :: Ui.M m => RulerId -> RulerId -> m ()
replaceRulerId old new = do
    blocks <- Ui.blocksWithRulerId old
    forM_ (map fst blocks) $ \blockId ->
        Ui.replaceRulerId blockId old new

-- * query

getMeter :: Ui.M m => BlockId -> m Meter.Meter
getMeter blockId = Ruler.getMeter <$> (Ui.getRuler =<< Ui.rulerOf blockId)

getSections :: Ui.M m => BlockId -> m [Meter.MSection]
getSections = fmap Meter.meterSections . getMeter

getMarks :: Ui.M m => BlockId -> m [(TrackTime, Mark.Mark)]
getMarks blockId =
    Mark.ascending 0 . Ruler.getMarklist Ruler.meterName <$>
        (Ui.getRuler =<< Ui.rulerOf blockId)

-- | Ruler under the selection having at least the given rank.
selectedMarks :: Cmd.M m => Meter.Rank -> m [(TrackTime, Mark.Mark)]
selectedMarks rank = do
    ruler <- Ui.getRuler =<< selected
    (start, end) <- selectionRange
    pure $ filter ((<=rank) . Mark.markRank . snd) $
        takeWhile ((<=end) . fst) $ Mark.ascending start $
        Ruler.getMarklist Ruler.meterName ruler

-- | Ruler of the track under the selection.
selected :: Cmd.M m => m RulerId
selected = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    Cmd.require "no ruler" =<< Ui.rulerTrackAt blockId tracknum

-- * upgrade to Meter.Meter

upgradeInfer :: Meter.AbstractMeter -> Upgrade
upgradeInfer meter mlist =
    ( RulerUtil.meterUntil meter dur 4 end
    , "measure " <> show dur <> " end: " <> show end
    )
    where (dur, end) = (inferMeasureDur mlist, Mark.end mlist)

inferMeasureDur :: Mark.Marklist -> TrackTime
inferMeasureDur mlist =
    maybe 0 fst $ Lists.maximumOn snd $ map (second length) $
        Lists.keyedGroupSort id $ zipWith subtract starts (drop 1 starts)
    where
    starts = map fst . filter ((<= Meter.W) . Mark.markRank . snd)
        . Mark.toList $ mlist

upgradeGong :: Upgrade
upgradeGong mlist = (Gong.until (Mark.end mlist), "")

type Upgrade = Mark.Marklist -> (Meter.Meter, String)

-- |
-- > LRuler.replace_meters False LRuler.upgrade_gong
-- > LRuler.replace_meters False (LRuler.upgrade_infer Meters.m44)
replaceMeters :: Ui.M m => Bool -> Upgrade -> m String
replaceMeters force upgrade = do
    rulerIds <- Ui.allRulerIds
    fmap unlines $ forM rulerIds $ \rulerId ->
        ((show rulerId <> ": ") <>) <$> replaceMeter force upgrade rulerId

-- | Add a Meter for the ruler if not already present.
replaceMeter :: Ui.M m => Bool -> Upgrade -> RulerId -> m String
replaceMeter force upgrade rulerId = do
    ruler <- Ui.getRuler rulerId
    case Map.lookup Ruler.meterName (Ruler.rulerMarklists ruler) of
        Just (oldMeter, mlist)
            | force || oldMeter == Nothing -> do
                let (meter, msg) = upgrade mlist
                RulerUtil.setMeter rulerId meter
                pure $ "replaced: " <> msg
            | otherwise -> pure "already has meter"
        Nothing -> pure "no meter?"

-- * Modify

-- | Copy the meter under the selection and append it to the end of the ruler.
append :: Cmd.M m => m Modify
append = do
    (start, end) <- selectionRange
    modifySelected $ Meter.modifySections $ \ss ->
        ss <> RulerUtil.extract start end ss

-- | Append another ruler to this one.
appendRulerId :: Cmd.M m => RulerId -> m Modify
appendRulerId rulerId = do
    other <- RulerUtil.getMeter rulerId
    modifySelected $ (<> other)

-- | Remove the selected range of the ruler and shift the rest up.
delete :: Cmd.M m => m Modify
delete = do
    (start, end) <- selectionRange
    modifySelected $ Meter.modifySections $ RulerUtil.delete start end

-- | Insert the selected meter range at the given time.
insert :: Cmd.M m => TrackTime -> m Modify
insert at = do
    (start, end) <- selectionRange
    sections <- getSections =<< Cmd.getFocusedBlock
    modifySelected $ Meter.setSections $
        Meter.sectionsTake at sections
        <> RulerUtil.extract start end sections
        <> Meter.sectionsDrop at sections

replaceRange :: TrackTime -> TrackTime -> [Meter.MSection] -> Meter.Meter
    -> Meter.Meter
replaceRange start end insert = Meter.modifySections $ \ss ->
    let pre = Meter.sectionsTake start ss
        post = Meter.sectionsDrop end ss
    in pre <> insert <> post

-- -- | Replace the selected region with another marklist.
-- replace_range :: TrackTime -> TrackTime -> Meter.LabeledMeter
--     -> Meter.LabeledMeter -> Meter.LabeledMeter
-- replace_range start end insert meter =
--     before <> Meter.take_before (end - start) insert <> after
--     where
--     before = Meter.take_before start meter
--     after = Meter.drop_until end meter

-- TODO This is no longer possible since that's now hardcoded at the Meter.Make
-- level.  Having to do it at all is no good though, I should not have to pay
-- for marks which are not visible.
-- strip_ranks :: Cmd.M m => Meter.Rank -> m Modify
-- strip_ranks max_rank =
--     modify_selected $ Meter.strip_ranks (Meter.name_to_rank max_rank)

type Sections = Int
type Measures = Int

-- | Set the ruler to a number of measures of the given meter, where each
-- measure gets 1t:
--
-- > LRuler.local $ LRuler.measures Meters.m44 4 4
-- > LRuler.modify $ LRuler.measures Meters.m34 4 8
measures :: Cmd.M m => Meter.AbstractMeter -> Sections -> Measures -> m Modify
measures meter sections measures = modifySections $ const $
    replicate sections $ Meter.MSection measures 1 meter

-- | Create gongs with 'Gong.gongs'.
gongs :: Cmd.M m => Gong.Gongs -> Gong.Jegogans -> m Modify
gongs sections jegog = ruler $ Gong.regular sections jegog

java :: Cmd.M m => Int -> m Modify
java lines = ruler $ Gong.java lines

-- | Replace the meter with the concatenation of the rulers of the given
-- blocks.  This is like 'extract' except it doesn't infer the blocks from the
-- calls and doesn't scale the extracted rulers.
concat :: Cmd.M m => [BlockId] -> m Modify
concat blockIds = do
    sections <- mconcat <$> mapM getSections blockIds
    modifySections (const sections)

-- * pull_up, push_down

-- | Extract the meter marklists from the sub-blocks called on the given
-- track, concatenate them, and replace the current meter with it.
pullUp :: Cmd.M m => m Modify
pullUp = do
    (blockId, tracknum, trackId, _) <- Selection.getInsert
    meter <- Extract.pullUp blockId trackId
    pure $ makeModify blockId tracknum $
        const $ Right $ Ruler.meterRuler meter

pushDown :: Cmd.M m => Bool -> m ()
pushDown recursive = do
    (blockId, _, trackId, _) <- Selection.getInsert
    if recursive
        then Extract.pushDownRecursive False blockId trackId
        else void $ Extract.pushDown False blockId trackId

-- * modify

-- | Change a Modify so it modifies only the selected tracks.
tracks :: Cmd.M m => m Modify -> m Modify
tracks modify = do
    modify <- modify
    (_, tracknums, _, _) <- Selection.tracks
    pure $ modify { mScope = RulerUtil.Tracks tracknums }

-- | Change a Modify so it modifies all rulers on the block.
block :: Cmd.M m => m Modify -> m Modify
block modify = do
    modify <- modify
    pure $ modify { mScope = RulerUtil.Block }

-- | Enough information to modify a ruler.
--
-- TODO I could also include entire block, and then add_cue etc. could use it,
-- in addition to being able to clip the entire block.
data Modify = Modify
    { mBlockId :: !BlockId
    , mScope :: !RulerUtil.Scope
    , mModify :: !RulerUtil.ModifyRuler
    }

modifySections :: Cmd.M m => ([Meter.MSection] -> [Meter.MSection]) -> m Modify
modifySections = modifySelected . Meter.modifySections

setStartMeasure :: Cmd.M m => Meter.Measures -> m Modify
setStartMeasure n = modifySelected $ Meter.modifyConfig $ \c ->
    c { Meter.configStartMeasure = n }

modifySelected :: Cmd.M m => (Meter.Meter -> Meter.Meter) -> m Modify
modifySelected modify = do
    (blockId, tracknum) <- getBlockTrack
    pure $ makeModify blockId tracknum (Right . Ruler.modifyMeter modify)

makeModify :: BlockId -> TrackNum -> RulerUtil.ModifyRuler -> Modify
makeModify blockId tracknum = Modify blockId (RulerUtil.Section tracknum)

getBlockTrack :: Cmd.M m => m (BlockId, TrackNum)
getBlockTrack = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    pure (blockId, tracknum)

local :: Cmd.M m => m Modify -> m [RulerId]
local = (localM =<<)

modify :: Cmd.M m => m Modify -> m ()
modify = (modifyM =<<)

-- | Modify a ruler or rulers, making a copy if they're shared with another
-- block.
localM :: Cmd.M m => Modify -> m [RulerId]
localM (Modify blockId scope modify) = RulerUtil.local scope blockId modify

-- | Modify the ruler on the focused block.  Other blocks with the same ruler
-- will also be modified.
modifyM :: Cmd.M m => Modify -> m ()
modifyM (Modify blockId scope modify) = RulerUtil.modify scope blockId modify

-- | Modify a local copy of the main block ruler.
localRuler :: Ui.M m => BlockId -> (Ruler.Ruler -> Ruler.Ruler) -> m RulerId
localRuler blockId modify =
    RulerUtil.localSection blockId 0 $ Right . modify

-- * bounds

-- | Set the block's logical start time to the selection.  Notes before this
-- will play before the start of the calling event.
setStart :: Cmd.M m => m RulerId
setStart = do
    (blockId, _, _, pos) <- Selection.getInsert
    localRuler blockId $ \ruler ->
        let (_, e) = Ruler.getBounds ruler
        in Ruler.setBounds (Just pos) e ruler

-- | Set the block's logical end time to the selection.  Notes after this will
-- play after the end of the calling event.
setEnd :: Cmd.M m => m RulerId
setEnd = do
    (blockId, _, _, pos) <- Selection.getInsert
    localRuler blockId $ \ruler ->
        let (s, _) = Ruler.getBounds ruler
        in Ruler.setBounds s (Just pos) ruler

-- * cue

-- | Drop a mark at the selected point in the \"cue\" ruler.
addCue :: Text -> Cmd.CmdL RulerId
addCue label = do
    (blockId, tracknum, _, pos) <- Selection.getInsert
    addCueAt blockId tracknum pos label

removeCues :: Cmd.CmdL ()
removeCues = do
    blockId <- Cmd.getFocusedBlock
    RulerUtil.modifyBlock blockId $ Right . Ruler.removeMarklist cueName

addCueAt :: BlockId -> TrackNum -> ScoreTime -> Text -> Cmd.CmdL RulerId
addCueAt blockId tracknum pos label =
    RulerUtil.localSection blockId tracknum $
        Right . Ruler.modifyMarklist cueName
            (Mark.insertMark pos (cueMark label))

cueMark :: Text -> Mark.Mark
cueMark label = Mark.Mark
    { markRank = minBound
    , markWidth = 2
    , markColor = Color.black
    , markName = label
    , markNameZoomLevel = 0
    , markZoomLevel = 0
    }

cueName :: Ruler.Name
cueName = "cue"


{-
TODO I can't restore this unless I expose Make config

-- * colors

-- | Used to adjust mark colors interactively.
reset_colors :: Cmd.CmdL ()
reset_colors = do
    block_id <- Cmd.get_focused_block
    ruler_id <- Ui.ruler_of block_id
    Ui.modify_ruler ruler_id (Right . set_colors meter_ranks)

set_colors :: [(Color.Color, Make.MarkWidth, Int)] -> Ruler.Ruler
    -> Ruler.Ruler
set_colors ranks ruler =
    Ruler.set_meter config
        (Ruler.marklist $ map (second set) $ Mark.ascending 0 mlist)
        ruler
    where
    (config, mlist) = Ruler.get_meter ruler
    set mark = case Lists.at ranks (Mark.mark_rank mark) of
        Nothing -> error $ "no color for rank: " <> show (Mark.mark_rank mark)
        Just (color, width, _) -> mark
            { Mark.mark_color = color
            , Mark.mark_width = width
            }

meter_ranks :: [(Color.Color, Make.MarkWidth, Int)]
meter_ranks =
    [ (a3 0.0 0.0 0.0, 3, 8)    -- section
    , (a3 0.2 0.1 0.0, 2, 8)    -- measure / whole

    , (a3 1.0 0.4 0.2, 2, 8)    -- half
    , (a2 1.0 0.4 0.2, 2, 8)    -- quarter

    , (a3 1.0 0.4 0.9, 1, 8)    -- 8th
    , (a2 1.0 0.4 0.9, 1, 8)    -- 16th

    , (a2 0.1 0.5 0.1, 1, 8)    -- 32nd
    , (a1 0.1 0.5 0.1, 1, 8)    -- 64th

    , (a2 0.0 0.0 0.0, 1, 8)    -- 128th
    , (a1 0.0 0.0 0.0, 1, 8)    -- 256th
    ]
    where
    a1 = alpha 0.2
    a2 = alpha 0.4
    a3 = alpha 0.55
    alpha a r g b = Color.rgba r g b a
-}

-- * util

-- | Ruler operations don't care about selection orientation.
selectionRange :: Cmd.M m => m (TrackTime, TrackTime)
selectionRange = Events.rangeTimes <$> Selection.range
