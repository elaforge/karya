-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Helper functions to be imported into Cmd.Repl.Environ.  Cmd.Repl.Environ
    must be interpreted since it's the \"top level\" module, so I put the
    library of commands in here.  An unqualified import in Cmd.Repl.Environ
    means this module is in scope at the REPL.

    Of course, REPL commands can use anything in scope in Cmd.Repl.Environ, not
    just these helpers.  That includes all the various cmd_* functions used by
    the keybindings and everything in State.  Also, keybindings can be invoked
    directly with the 'keybinding' helper.  TODO not implemented

    Functions which are not designed to be composed generally take simpler
    types like strings, or get their block from the current focus, so they're
    easier to type.

    To keep this module from getting huge, only general purpose and common cmds
    should go here.  Cmds which are meant to be used from the REPL but may be
    more specialized can go in Cmd.Repl.L* modules.
-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Cmd.Repl.Global (
    module Cmd.Repl.Global, module Cmd.ModifyEvents
) where
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
-- Just make sure these are compiled.
import Midi.Synth ()
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
-- These are used to write patterns for 'ModifyEvents.substitute'.
import Cmd.ModifyEvents (Replacement(F), w, ws, ws1)
import Cmd.Repl.LEvent ()
import qualified Cmd.Repl.LInst as LInst
import Cmd.Repl.LPerf ()
import Cmd.Repl.LPitch ()
import Cmd.Repl.LRuler ()
import Cmd.Repl.LTrack ()
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global
import Types


-- | Take a string and automatically figure out what kind of ID is expected and
-- add a namespace if one was not already in the string.
--
-- Throws an error if the ID has bad characters, which is ok since this is
-- expected to be used from the REPL.
--
-- This is used by the REPL's macro feature, to replace @xyz@ with
-- (make_id "current-namespace" "xyz")
make_id :: Id.Ident a => Text -> Text -> a
make_id ns name
    | valid = Id.make id
    | otherwise = error $ "invalid characters in id: " ++ show name
    where (id, valid) = Id.read_short_validate (Id.namespace ns) name

vid :: Text -> ViewId
vid = Id.ViewId . Id.read_id

bid :: Text -> BlockId
bid = Id.BlockId . Id.read_id

rid :: Text -> RulerId
rid = Id.RulerId . Id.read_id

tid :: Text -> TrackId
tid = Id.TrackId . Id.read_id

-- | Get the current focused block.
block :: Cmd.CmdL BlockId
block = Cmd.get_focused_block

-- | Get the track under the selection.
track :: Cmd.CmdL TrackId
track = do
    (_, _, track_id, _) <- Selection.get_insert
    return track_id

tracknum :: Cmd.CmdL TrackNum
tracknum = do
    (_, tracknum, _, _) <- Selection.get_insert
    return tracknum

-- | Get the current focused view.
view :: Cmd.CmdL ViewId
view = Cmd.get_focused_view

-- | RulerId of the ruler under the selection.
ruler :: Cmd.CmdL RulerId
ruler = do
    n <- tracknum
    block_id <- block
    Cmd.abort_unless =<< State.ruler_track_at block_id n

-- | Get the root block.
root :: Cmd.CmdL BlockId
root = State.get_root_id

-- | Create a namespace, and throw an IO exception if it has bad characters.
-- Intended to be used from the REPL, where throwing an IO exception is ok.
ns :: Text -> Id.Namespace
ns name
    | Id.valid name = Id.namespace name
    | otherwise = error $ "bad namespace: " ++ show name

-- | Some oprators to more conveniently string together monadic and non-monadic
-- functions in the REPL.
--
-- For instance:
--
-- @
--      block >>= LPerf.get_midi_cache $> Midi.Cache.cache_chunks
--              .> (!!1) .> Midi.Cache.chunk_state .> Perform.state_postproc
-- @
($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip (<$>)
infixl 1 $> -- put it above ($) but below everything else

-- | The REPL puts haskell in your sequencer, so you can compose while you
-- compose.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>

-- | Pretty-print the result of a cmd with 'Pretty.format'.
pp :: Pretty.Pretty a => Cmd.CmdL a -> Cmd.CmdL Text
pp = fmap Pretty.formatted

quit :: Cmd.CmdL ()
quit = Cmd.modify $ \st -> st { Cmd.state_repl_status = Cmd.Quit }

-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s "" = unerror
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " <> showt stackpos)
    highlight_error (Stack.parse_ui_frame stackpos)

highlight_error :: Stack.UiFrame -> Cmd.CmdL ()
highlight_error (maybe_bid, maybe_tid, maybe_range) = do
    unerror
    block_id <- maybe find_block return maybe_bid
    view_ids <- Map.keys <$> State.views_of block_id
    view_ids <- if null view_ids then (:[]) <$> Create.view block_id
        else return view_ids
    mapM_ Cmd.focus view_ids
    case (maybe_tid, maybe_range) of
        (Nothing, _) -> forM_ view_ids $ \vid ->
            Selection.set_selnum vid Config.error_selnum
                (Just (Sel.selection 0 0 9999 9999))
        (Just tid, Nothing) -> do
            tracknum <- State.get_tracknum_of block_id tid
            forM_ view_ids $ \vid ->
                Selection.set_selnum vid Config.error_selnum
                    (Just (Sel.selection tracknum 0 tracknum 9999))
        (Just tid, Just (from, to)) -> do
            tracknum <- State.get_tracknum_of block_id tid
            forM_ view_ids $ \vid ->
                Selection.set_and_scroll vid Config.error_selnum
                    (Sel.selection tracknum to tracknum from)
    where
    find_block = case maybe_tid of
        Nothing -> Cmd.throw $
            "can't highlight stack frame with neither block nor track: "
            <> showt (maybe_bid, maybe_tid, maybe_range)
        Just track_id -> maybe (Cmd.throw $ "no block with " <> showt track_id)
            (return . fst) . Seq.head
                =<< State.blocks_with_track_id track_id

unerror :: Cmd.CmdL ()
unerror = do
    view_ids <- State.all_view_ids
    forM_ view_ids $ \vid ->
        Selection.set_selnum vid Config.error_selnum Nothing

-- * show / modify cmd state

show_history :: Cmd.CmdL Text
show_history = do
    hist <- Cmd.gets Cmd.state_history
    return $ Pretty.formatted hist

-- * load / save

save :: Cmd.CmdL ()
save = Save.save

-- | Save to the given filename and switch to saving plain states.
save_state :: Cmd.CmdL ()
save_state = Save.save_state

-- | Save to the given filename and switch to saving plain states.
save_state_as :: FilePath -> Cmd.CmdL ()
save_state_as = Save.save_state_as

-- | Like 'save_state', but don't change 'Cmd.state_save_file'.
write_state :: FilePath -> Cmd.CmdL FilePath
write_state = Save.write_current_state

save_git :: Cmd.CmdL ()
save_git = Save.save_git

-- | Save to the given git repo and switch to saving incrementally.
save_git_as :: FilePath -> Cmd.CmdL ()
save_git_as = Save.save_git_as

load :: FilePath -> Cmd.CmdL ()
load = Save.load

revert :: Cmd.CmdL ()
revert = Save.revert Nothing

revert_to :: String -> Cmd.CmdL ()
revert_to = Save.revert . Just

-- * called externally

collapse_track, expand_track :: BlockId -> TrackNum -> Cmd.CmdL ()
collapse_track block_id tracknum = do
    -- TODO if the track to collapse is a pitch track, merge it with its
    -- note track instead
    State.add_track_flag block_id tracknum Block.Collapse
    Info.set_instrument_status block_id tracknum
expand_track block_id tracknum = do
    State.remove_track_flag block_id tracknum Block.Collapse
    Info.set_instrument_status block_id tracknum

-- | Called from logview.
collapse, expand :: TrackNum -> Cmd.CmdL ()
collapse tracknum = flip collapse_track tracknum =<< Cmd.get_focused_block
expand tracknum = flip expand_track tracknum =<< Cmd.get_focused_block

-- | Called from the browser.
change_instrument :: Text -> Cmd.CmdL ()
change_instrument = LInst.change_instrument

-- | The result of a REPL Cmd is converted to a 'ReplProtocol.Result' with
-- this method.
class Return a where
    _to_result :: a -> ReplProtocol.Result
instance {-# OVERLAPPABLE #-} Show a => Return a where
    _to_result = ReplProtocol.Format . showt
instance Return ReplProtocol.Result where
    _to_result = id
instance Return Text where
    _to_result = ReplProtocol.Raw
instance Return String where
    _to_result = ReplProtocol.Raw . txt
instance Return () where
    _to_result () = ReplProtocol.Raw ""
