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
    block_ids <- Ui.all_block_ids
    view_blocks <- Ui.gets $ map Block.view_block . Map.elems . Ui.state_views
    return
        [ (block_id, Lists.count (==block_id) view_blocks)
        | block_id <- block_ids
        ]

-- | Find BlockIds that match the string.
find_id :: Ui.M m => Text -> m [BlockId]
find_id match = filter (Util.match_id match) <$>
    Ui.gets (Map.keys . Ui.state_blocks)

pretty :: Ui.M m => BlockId -> m Text
pretty block_id = do
    block <- Ui.get_block block_id
    tracks <- Ui.gets Ui.state_tracks
    view_blocks <- Ui.gets $ map Block.view_block . Map.elems . Ui.state_views
    return $ Pretty.formatted $
        pretty_tracks view_blocks tracks block_id block
    where
    pretty_tracks view_blocks tracks block_id block =
        Pretty.format (Block.block_title block)
            Pretty.<+> Pretty.text ("(" <> showt views <> " views)")
            Pretty.<+> Pretty.textList (map track (Block.block_tracks block))
        where
        track t = Pretty.pretty (Block.tracklike_id t)
            <> " (" <> showt (track_events t) <> " events)"
        views = Lists.count (==block_id) view_blocks
        get = flip Map.lookup tracks <=< Block.track_id
        track_events = maybe 0 (Events.length . Track.track_events) . get

-- | Find all blocks with the given text in their titles.
find :: Text -> Cmd.CmdL [(BlockId, Text)]
find substr = find_f (substr `Text.isInfixOf`)

find_f :: (Text -> Bool) -> Cmd.CmdL [(BlockId, Text)]
find_f match = do
    block_ids <- Ui.all_block_ids
    titles <- mapM Ui.get_block_title block_ids
    return [(block_id, title) | (block_id, title) <- zip block_ids titles,
        match title]

-- | Transform all block titles.
map_titles :: (Text -> Text) -> Cmd.CmdL ()
map_titles modify = do
    block_ids <- Ui.all_block_ids
    titles <- mapM Ui.get_block_title block_ids
    forM_ (zip block_ids titles) $ \(block_id, title) ->
        Ui.set_block_title block_id (modify title)

replace_titles :: Text -> Text -> Cmd.CmdL ()
replace_titles from to = map_titles $ Text.replace from to

set_all_implicit :: Cmd.M m => m ()
set_all_implicit = do
    block_ids <- Ui.gets $ Map.keys . Ui.state_blocks
    mapM_ (\b -> Ui.set_skeleton_config b Block.Implicit) block_ids

set_explicit :: Cmd.M m => m ()
set_explicit = do
    block_id <- Cmd.get_focused_block
    Ui.set_skeleton_config block_id Block.Explicit

set_implicit :: Cmd.M m => m ()
set_implicit = do
    block_id <- Cmd.get_focused_block
    Ui.set_skeleton_config block_id Block.Implicit

-- * doc

doc :: Cmd.CmdL Text
doc = Lazy.toStrict . CallDoc.doc_text <$> track_doc

doc_equal :: CallDoc.SymbolName -> Cmd.CmdL Text
doc_equal name = find_doc (\sym _call -> sym == name)

doc_like :: Text -> Cmd.CmdL Text
doc_like pattern = find_doc $ \sym (Derive.CallName call) ->
    pattern `Text.isInfixOf` sym || pattern `Text.isInfixOf` call

find_doc :: (CallDoc.SymbolName -> Derive.CallName -> Bool) -> Cmd.CmdL Text
find_doc matches  = Lazy.toStrict . CallDoc.doc_text
    . CallDoc.filter_calls matches <$> track_doc

-- | Write HTML documentation for the selected track to
-- @build/derive_doc.html@.
html_doc :: Cmd.CmdL ()
html_doc = do
    doc <- track_doc
    app_dir <- Cmd.gets (Cmd.config_app_dir . Cmd.state_config)
    hstate <- liftIO $ Html.get_html_state "haddock" app_dir
    liftIO $ Text.IO.writeFile "build/derive_doc.html" $
        Html.un_html $ CallDoc.doc_html hstate doc

-- | Print a summary of bindings in scope, grouped by namespace and sorted by
-- shadow priority.  This is useful to see if your call is being shadowed.
--
-- If the same call shows up twice it may mean you imported the same module
-- twice.
bindings_equal :: CallDoc.SymbolName -> Cmd.CmdL Text
bindings_equal name = find_bindings (==name)

find_bindings :: (CallDoc.SymbolName -> Bool) -> Cmd.CmdL Text
find_bindings matches = CallDoc.bindings_text
    . CallDoc.filter_calls (\sym _call -> matches sym) <$> track_doc

track_doc :: Cmd.CmdL CallDoc.Document
track_doc = do
    (block_id, _, track_id, _) <- Selection.get_insert
    CallDoc.track block_id track_id

-- * block call

-- | Rename focused block.
--
-- It doesn't update TrackIds so they may still be named under their old block,
-- but track id names aren't supposed to carry meaning anyway.
rename :: Id.Id -> Cmd.CmdL ()
rename to = flip Create.rename_block to =<< Cmd.get_focused_block

-- | Rename a block and update all calls to it in all blocks.  This is not
-- totally accurate since it updates all symbols that match, but it doesn't
-- know that the symbol would be definitely used as a block call.  So if you
-- have @clef = treble@ and a block named @treble@, it will update both.  I
-- could probably solve this by switching back to separate string and symbol
-- types, but it seems like a minor issue.
rename_all :: BlockId -> Id.Id -> Cmd.CmdL ()
rename_all from to = do
    Create.rename_block from to
    ModifyEvents.note_tracks $ ModifyEvents.text $ replace_block_call from to

-- | Rename block calls in a single block.
replace :: BlockId -> Id.Id -> Cmd.CmdL ()
replace from to = do
    block_id <- Cmd.get_focused_block
    ModifyEvents.block block_id $
        ModifyEvents.tracks_named ParseTitle.is_note_track $
        ModifyEvents.text $ replace_block_call from to

replace_block_call :: BlockId -> Id.Id -> Text -> Text
replace_block_call from to = map_symbol replace
    where
    replace sym
        | sym == Id.ident_name from = Id.ident_name to
        | sym == Id.ident_text from = Id.ident_text to
        | otherwise = sym

map_symbol :: (Text -> Text) -> Text -> Text
map_symbol f text =
    either (const text) (ShowVal.show_val . map_text f) (Parse.parse_expr text)

-- | Transform both Symbols and Strs.
map_text :: (Text -> Text) -> DeriveT.Expr -> DeriveT.Expr
map_text f = fmap $ Expr.map_symbol (Expr.Symbol . f . Expr.unsym)
    . DeriveT.map_str (Expr.Str . f . Expr.unstr)

-- * create

-- | If the events under the cursor are a block calls, create blocks that don't
-- already exist.  Optionally use a template block.
for_event :: Maybe BlockId -> Cmd.CmdL ()
for_event maybe_template = mapM_ make =<< Selection.events
    where
    make (_, events) = mapM_ make1 events
    make1 event = do
        id <- Ui.read_id (Event.text event)
        maybe named named_from maybe_template id

-- | Copy the current block into a new empty block with the given name.
named :: Id.Id -> Cmd.CmdL ViewId
named name = flip named_from name =<< Cmd.get_focused_block

named_from :: BlockId -> Id.Id -> Cmd.CmdL ViewId
named_from template_id name =
    Create.view =<< Create.named_block_from_template False template_id name

-- | Create a named block with the same structure as the focused one.
copy :: Bool -> Id.Id -> Cmd.CmdL ViewId
copy copy_events name = do
    block_id <- Cmd.get_focused_block
    Create.view =<< Create.named_block_from_template copy_events block_id name

-- * destroy

destroy :: Ui.M m => [BlockId] -> m ()
destroy = mapM_ Create.destroy_block

destroy_except :: [BlockId] -> Cmd.CmdL ()
destroy_except keep = do
    block_ids <- Ui.all_block_ids
    mapM_ Create.destroy_block (filter (not . (`elem` keep)) block_ids)

-- * dividers

-- | Insert a divider to the right of the selection.
divide :: Cmd.CmdL ()
divide = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    Ui.insert_track block_id (tracknum+1) Block.divider

-- | Remove a divider to the right of the selection.  The selection likes to
-- skip dividers so they can't be deleted normally.
undivide :: Cmd.CmdL ()
undivide = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    tracks <- Block.block_tracks <$> Ui.get_block block_id
    let found = List.find ((==Block.divider) . snd)
            (drop tracknum (zip [0..] tracks))
    whenJust found $ \(n, _) -> Ui.remove_track block_id n

collapse_children :: Cmd.M m => m ()
collapse_children = do
    (block_id, _, track_id, _) <- Selection.get_insert
    BlockConfig.collapse_children block_id track_id

expand_children :: Cmd.M m => m ()
expand_children = do
    (block_id, _, track_id, _) <- Selection.get_insert
    BlockConfig.expand_children block_id track_id

-- * merge

append :: Cmd.M m => BlockId -> m ()
append source = do
    dest <- Cmd.get_focused_block
    BlockConfig.append dest source

create_merged :: Cmd.M m => BlockId -> BlockId -> m ViewId
create_merged b1 b2 = do
    ruler_id <- Ui.block_ruler b1
    new <- Create.block ruler_id
    BlockConfig.append new b1
    BlockConfig.append new b2
    Create.view new

-- * stretch

stretch_block :: ScoreTime -> BlockId -> Cmd.CmdL ()
stretch_block factor block_id = ModifyEvents.block block_id $
    ModifyEvents.event (stretch factor)

stretch :: ScoreTime -> Event.Event -> Event.Event
stretch factor = (Event.start_ %= (*factor)) . (Event.duration_ %= (*factor))

-- * add / remove time

-- | Add time encompassed by the selection to this block, expand its event in
-- caller blocks, and renumber the ruler globally.  This is like a super
-- 'Cmd.Edit.cmd_insert_time'.
add_time :: Cmd.M m => m ()
add_time = do
    sel <- Selection.get
    block_id <- Cmd.get_focused_block
    updates <- BlockResize.update_callers block_id (Sel.min sel)
        (Sel.duration sel)
    BlockResize.push_down_rulers updates

remove_time :: Cmd.M m => m ()
remove_time = do
    sel <- Selection.get
    block_id <- Cmd.get_focused_block
    updates <- BlockResize.update_callers block_id (Sel.min sel)
        (- Sel.duration sel)
    BlockResize.push_down_rulers updates

-- | Like 'add_time' and 'remove_time', except this will splice the selected
-- bit of ruler into the corresponding times in the top track.  This is useful
-- if there are changing time signatures, and you want to move future time
-- signatures along with the events.
add_time_ruler :: Cmd.M m => m [BlockId]
add_time_ruler = do
    sel <- Selection.get
    block_id <- Cmd.get_focused_block
    BlockResize.update_callers_rulers block_id (Sel.min sel) (Sel.duration sel)

remove_time_ruler :: Cmd.M m => m [BlockId]
remove_time_ruler = do
    sel <- Selection.get
    block_id <- Cmd.get_focused_block
    BlockResize.update_callers_rulers block_id (Sel.min sel)
        (- Sel.duration sel)
