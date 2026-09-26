-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Block level cmds.
module Cmd.Repl.LBlock where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Lazy

import qualified Util.Html as Html
import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty

import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.BlockResize as BlockResize
import qualified Cmd.CallDoc as CallDoc
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ShowVal as ShowVal

import           Global hiding (pretty)
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Types


-- | All BlockIds, along with the count of views for each one.
list :: Cmd.CmdL [(BlockId, Int)]
list = do
    blockIds <- Ui.allBlockIds
    viewBlocks <- Ui.gets $ map Block.viewBlock . Map.elems . Ui.stateViews
    pure
        [ (blockId, Lists.count (==blockId) viewBlocks)
        | blockId <- blockIds
        ]

-- | Find BlockIds that match the string.
findId :: Ui.M m => Text -> m [BlockId]
findId match = filter (Util.matchId match) <$>
    Ui.gets (Map.keys . Ui.stateBlocks)

pretty :: Ui.M m => BlockId -> m Text
pretty blockId = do
    block <- Ui.getBlock blockId
    tracks <- Ui.gets Ui.stateTracks
    viewBlocks <- Ui.gets $ map Block.viewBlock . Map.elems . Ui.stateViews
    pure $ Pretty.formatted $
        prettyTracks viewBlocks tracks blockId block
    where
    prettyTracks viewBlocks tracks blockId block =
        Pretty.format (Block.blockTitle block)
            Pretty.<+> Pretty.text ("(" <> showt views <> " views)")
            Pretty.<+> Pretty.textList (map track (Block.blockTracks block))
        where
        track t = Pretty.pretty (Block.tracklikeId t)
            <> " (" <> showt (trackEvents t) <> " events)"
        views = Lists.count (==blockId) viewBlocks
        get = flip Map.lookup tracks <=< Block.trackId
        trackEvents = maybe 0 (Events.length . Track.trackEvents) . get

-- | Find all blocks with the given text in their titles.
find :: Text -> Cmd.CmdL [(BlockId, Text)]
find substr = findF (substr `Text.isInfixOf`)

findF :: (Text -> Bool) -> Cmd.CmdL [(BlockId, Text)]
findF match = do
    blockIds <- Ui.allBlockIds
    titles <- mapM Ui.getBlockTitle blockIds
    pure [(blockId, title) | (blockId, title) <- zip blockIds titles,
        match title]

-- | Transform all block titles.
mapTitles :: (Text -> Text) -> Cmd.CmdL ()
mapTitles modify = do
    blockIds <- Ui.allBlockIds
    titles <- mapM Ui.getBlockTitle blockIds
    forM_ (zip blockIds titles) $ \(blockId, title) ->
        Ui.setBlockTitle blockId (modify title)

replaceTitles :: Text -> Text -> Cmd.CmdL ()
replaceTitles from to = mapTitles $ Text.replace from to

setAllImplicit :: Cmd.M m => m ()
setAllImplicit = do
    blockIds <- Ui.gets $ Map.keys . Ui.stateBlocks
    mapM_ (\b -> Ui.setSkeletonConfig b Block.Implicit) blockIds

setExplicit :: Cmd.M m => m ()
setExplicit = do
    blockId <- Cmd.getFocusedBlock
    Ui.setSkeletonConfig blockId Block.Explicit

setImplicit :: Cmd.M m => m ()
setImplicit = do
    blockId <- Cmd.getFocusedBlock
    Ui.setSkeletonConfig blockId Block.Implicit

-- * doc

doc :: Cmd.CmdL Text
doc = Lazy.toStrict . CallDoc.docText <$> trackDoc

docEqual :: CallDoc.SymbolName -> Cmd.CmdL Text
docEqual name = findDoc (\sym _call -> sym == name)

docLike :: Text -> Cmd.CmdL Text
docLike pattern = findDoc $ \sym (Derive.CallName call) ->
    pattern `Text.isInfixOf` sym || pattern `Text.isInfixOf` call

findDoc :: (CallDoc.SymbolName -> Derive.CallName -> Bool) -> Cmd.CmdL Text
findDoc matches  = Lazy.toStrict . CallDoc.docText
    . CallDoc.filterCalls matches <$> trackDoc

-- | Write HTML documentation for the selected track to
-- @build/derive_doc.html@.
htmlDoc :: Cmd.CmdL ()
htmlDoc = do
    doc <- trackDoc
    appDir <- Cmd.gets (Cmd.configAppDir . Cmd.stateConfig)
    hstate <- liftIO $ Html.getHtmlState "haddock" appDir
    liftIO $ Text.IO.writeFile "build/derive_doc.html" $
        Html.unHtml $ CallDoc.docHtml hstate doc

-- | Print a summary of bindings in scope, grouped by namespace and sorted by
-- shadow priority.  This is useful to see if your call is being shadowed.
--
-- If the same call shows up twice it may mean you imported the same module
-- twice.
bindingsEqual :: CallDoc.SymbolName -> Cmd.CmdL Text
bindingsEqual name = findBindings (==name)

findBindings :: (CallDoc.SymbolName -> Bool) -> Cmd.CmdL Text
findBindings matches = CallDoc.bindingsText
    . CallDoc.filterCalls (\sym _call -> matches sym) <$> trackDoc

trackDoc :: Cmd.CmdL CallDoc.Document
trackDoc = do
    (blockId, _, trackId, _) <- Selection.getInsert
    CallDoc.track blockId trackId

-- * block call

-- | Rename focused block.
--
-- It doesn't update TrackIds so they may still be named under their old block,
-- but track id names aren't supposed to carry meaning anyway.
rename :: Id.Id -> Cmd.CmdL ()
rename to = flip Create.renameBlock to =<< Cmd.getFocusedBlock

-- | Rename a block and update all calls to it in all blocks.  This is not
-- totally accurate since it updates all symbols that match, but it doesn't
-- know that the symbol would be definitely used as a block call.  So if you
-- have @clef = treble@ and a block named @treble@, it will update both.  I
-- could probably solve this by switching back to separate string and symbol
-- types, but it seems like a minor issue.
renameAll :: BlockId -> Id.Id -> Cmd.CmdL ()
renameAll from to = do
    Create.renameBlock from to
    ModifyEvents.noteTracks $ ModifyEvents.text $ replaceBlockCall from to

-- | Rename block calls in a single block.
replace :: BlockId -> Id.Id -> Cmd.CmdL ()
replace from to = do
    blockId <- Cmd.getFocusedBlock
    ModifyEvents.block blockId $
        ModifyEvents.tracksNamed ParseTitle.isNoteTrack $
        ModifyEvents.text $ replaceBlockCall from to

replaceBlockCall :: BlockId -> Id.Id -> Text -> Text
replaceBlockCall from to = mapSymbol replace
    where
    replace sym
        | sym == Id.identName from = Id.identName to
        | sym == Id.identText from = Id.identText to
        | otherwise = sym

mapSymbol :: (Text -> Text) -> Text -> Text
mapSymbol f text =
    either (const text) (ShowVal.showVal . mapText f) (Parse.parseExpr text)

-- | Transform both Symbols and Strs.
mapText :: (Text -> Text) -> DeriveT.Expr -> DeriveT.Expr
mapText f = fmap $ Expr.mapSymbol (Expr.Symbol . f . Expr.unsym)
    . DeriveT.mapStr (Expr.Str . f . Expr.unstr)

-- * create

-- | If the events under the cursor are a block calls, create blocks that don't
-- already exist.  Optionally use a template block.
forEvent :: Maybe BlockId -> Cmd.CmdL ()
forEvent maybeTemplate = mapM_ make =<< Selection.events
    where
    make (_, events) = mapM_ make1 events
    make1 event = do
        id <- Ui.readId (Event.text event)
        maybe named namedFrom maybeTemplate id

-- | Copy the current block into a new empty block with the given name.
named :: Id.Id -> Cmd.CmdL ViewId
named name = flip namedFrom name =<< Cmd.getFocusedBlock

namedFrom :: BlockId -> Id.Id -> Cmd.CmdL ViewId
namedFrom templateId name =
    Create.view =<< Create.namedBlockFromTemplate False templateId name

-- | Create a named block with the same structure as the focused one.
copy :: Bool -> Id.Id -> Cmd.CmdL ViewId
copy copyEvents name = do
    blockId <- Cmd.getFocusedBlock
    Create.view =<< Create.namedBlockFromTemplate copyEvents blockId name

-- * destroy

destroy :: Ui.M m => [BlockId] -> m ()
destroy = mapM_ Create.destroyBlock

destroyExcept :: [BlockId] -> Cmd.CmdL ()
destroyExcept keep = do
    blockIds <- Ui.allBlockIds
    mapM_ Create.destroyBlock (filter (not . (`elem` keep)) blockIds)

-- * dividers

-- | Insert a divider to the right of the selection.
divide :: Cmd.CmdL ()
divide = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    Ui.insertTrack blockId (tracknum+1) Block.divider

-- | Remove a divider to the right of the selection.  The selection likes to
-- skip dividers so they can't be deleted normally.
undivide :: Cmd.CmdL ()
undivide = do
    (blockId, tracknum, _, _) <- Selection.getInsert
    tracks <- Block.blockTracks <$> Ui.getBlock blockId
    let found = List.find ((==Block.divider) . snd)
            (drop tracknum (zip [0..] tracks))
    whenJust found $ \(n, _) -> Ui.removeTrack blockId n

collapseChildren :: Cmd.M m => m ()
collapseChildren = do
    (blockId, _, trackId, _) <- Selection.getInsert
    BlockConfig.collapseChildren blockId trackId

expandChildren :: Cmd.M m => m ()
expandChildren = do
    (blockId, _, trackId, _) <- Selection.getInsert
    BlockConfig.expandChildren blockId trackId

-- * merge

append :: Cmd.M m => BlockId -> m ()
append source = do
    dest <- Cmd.getFocusedBlock
    BlockConfig.append dest source

createMerged :: Cmd.M m => BlockId -> BlockId -> m ViewId
createMerged b1 b2 = do
    rulerId <- Ui.blockRuler b1
    new <- Create.block rulerId
    BlockConfig.append new b1
    BlockConfig.append new b2
    Create.view new

-- * stretch

stretchBlock :: ScoreTime -> BlockId -> Cmd.CmdL ()
stretchBlock factor blockId = ModifyEvents.block blockId $
    ModifyEvents.event (stretch factor)

stretch :: ScoreTime -> Event.Event -> Event.Event
stretch factor = (Event.start_ %= (*factor)) . (Event.duration_ %= (*factor))

-- * add / remove time

-- | Add time encompassed by the selection to this block, expand its event in
-- caller blocks, and renumber the ruler globally.  This is like a super
-- 'Cmd.Edit.cmd_insert_time'.
addTime :: Cmd.M m => m ()
addTime = do
    sel <- Selection.get
    blockId <- Cmd.getFocusedBlock
    updates <- BlockResize.updateCallers blockId (Sel.min sel)
        (Sel.duration sel)
    BlockResize.pushDownRulers updates

removeTime :: Cmd.M m => m ()
removeTime = do
    sel <- Selection.get
    blockId <- Cmd.getFocusedBlock
    updates <- BlockResize.updateCallers blockId (Sel.min sel)
        (- Sel.duration sel)
    BlockResize.pushDownRulers updates

-- | Like 'add_time' and 'remove_time', except this will splice the selected
-- bit of ruler into the corresponding times in the top track.  This is useful
-- if there are changing time signatures, and you want to move future time
-- signatures along with the events.
addTimeRuler :: Cmd.M m => m [BlockId]
addTimeRuler = do
    sel <- Selection.get
    blockId <- Cmd.getFocusedBlock
    BlockResize.updateCallersRulers blockId (Sel.min sel) (Sel.duration sel)

removeTimeRuler :: Cmd.M m => m [BlockId]
removeTimeRuler = do
    sel <- Selection.get
    blockId <- Cmd.getFocusedBlock
    BlockResize.updateCallersRulers blockId (Sel.min sel)
        (- Sel.duration sel)
