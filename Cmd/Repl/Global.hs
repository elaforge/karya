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

    The various show_* functions print out state generally in a 'show' format,
    but not necessarily.  It's designed to be for human reading and may leave
    out relatively uninteresting data.

    TODO Can I use Language.Haskell.Parser or haddock to generate a list of
    toplevel names along with their documentation to give the REPL for
    completion and interactive documentation?

    To keep this module from getting huge, only general purpose and common cmds
    should go here.  Cmds which are meant to be used from the REPL but may be
    more specialized can go in Cmd.Repl.L* modules.
-}
module Cmd.Repl.Global where
import qualified Data.Map as Map

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

-- Just make sure these are compiled.
import Midi.Synth ()
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
import Cmd.Repl.LEvent ()
import qualified Cmd.Repl.LInst as LInst
import Cmd.Repl.LPerf ()
import Cmd.Repl.LPitch ()
import Cmd.Repl.LRuler ()
import Cmd.Repl.LTrack ()
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Cmd.ViewConfig as ViewConfig

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import Types


-- * util

-- | Take a string and automatically figure out what kind of ID is expected and
-- add a namespace if one was not already in the string.
--
-- Throws an error if the ID has bad characters, which is ok since this is
-- expected to be used from the REPL.
class AutoId a where auto_id :: String -> String -> a
instance AutoId ViewId where auto_id = make_id Types.ViewId "ViewId"
instance AutoId BlockId where auto_id = make_id Types.BlockId "BlockId"
instance AutoId RulerId where auto_id = make_id Types.RulerId "RulerId"
instance AutoId TrackId where auto_id = make_id Types.TrackId "TrackId"

make_id :: (Id.Id -> a) -> String -> String -> String -> a
make_id make msg ns name = case Id.namespace ns of
    Nothing -> error $ msg ++ ": illegal characters in namespace: " ++ show ns
    Just ns -> case Id.read_short ns name of
        Nothing -> error $ msg ++ ": illegal characters in ID: " ++ show name
        Just ident -> make ident

vid = Types.ViewId . Id.read_id
bid = Types.BlockId . Id.read_id
rid = Types.RulerId . Id.read_id
tid = Types.TrackId . Id.read_id

block :: Cmd.CmdL BlockId
block = Cmd.get_focused_block

view :: Cmd.CmdL ViewId
view = Cmd.get_focused_view

root :: Cmd.CmdL BlockId
root = State.get_root_id

-- | Create a namespace, and throw an IO exception if it has bad characters.
-- Intended to be used from the REPL, where throwing an IO exception is ok.
ns :: String -> Id.Namespace
ns s = case Id.namespace s of
    Nothing -> error $ "bad namespace: " ++ show s
    Just ns -> ns

-- | Some oprators to more conveniently string together monadic and non-monadic
-- functions in the REPL.
--
-- For instance:
--
-- @
--      block >>= LPerf.get_midi_cache $> Midi.Cache.cache_chunks
--              .> (!!1) .> Midi.Cache.chunk_state .> Perform.state_postproc
-- @

($>) :: (Functor f) => f a -> (a -> b) -> f b
($>) = flip (<$>)
infixl 1 $> -- put it above ($) but below everything else

-- | The REPL puts haskell in your sequencer, so you can compose while you
-- compose.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>

-- | Pretty-print the result of a cmd with 'Pretty.format'.
pp :: (Pretty.Pretty a) => Cmd.CmdL a -> Cmd.CmdL String
pp = fmap Pretty.formatted

quit :: Cmd.CmdL ()
quit = Cmd.modify $ \st -> st { Cmd.state_repl_status = Cmd.Quit }

-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s "" = unerror
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " ++ show stackpos)
        highlight_error (Stack.parse_ui_frame stackpos)

highlight_error :: Stack.UiFrame -> Cmd.CmdL ()
highlight_error (maybe_bid, maybe_tid, maybe_range) = do
    unerror
    block_id <- maybe find_block return maybe_bid
    view_ids <- Map.keys <$> State.views_of block_id
    view_ids <- if null view_ids then (:[]) <$> Create.view block_id
        else return view_ids
    mapM_ ViewConfig.bring_to_front view_ids
    case (maybe_tid, maybe_range) of
        (Nothing, _) -> forM_ view_ids $ \vid ->
            Selection.set_selnum vid Config.error_selnum
                (Just (Types.selection 0 0 9999 9999))
        (Just tid, Nothing) -> do
            tracknum <- State.get_tracknum_of block_id tid
            forM_ view_ids $ \vid ->
                Selection.set_selnum vid Config.error_selnum
                    (Just (Types.selection tracknum 0 tracknum 9999))
        (Just tid, Just (from, to)) -> do
            tracknum <- State.get_tracknum_of block_id tid
            forM_ view_ids $ \vid ->
                Selection.set_and_scroll vid Config.error_selnum
                    (Types.selection tracknum to tracknum from)
    where
    find_block = case maybe_tid of
        Nothing -> Cmd.throw $
            "can't highlight stack frame with neither block nor track: "
            <> show (maybe_bid, maybe_tid, maybe_range)
        Just track_id -> maybe (Cmd.throw $ "no block with " <> show track_id)
            (return . fst) . Seq.head
                =<< State.blocks_with_track_id track_id

unerror :: Cmd.CmdL ()
unerror = do
    view_ids <- State.all_view_ids
    forM_ view_ids $ \vid ->
        Selection.set_selnum vid Config.error_selnum Nothing

-- * show / modify cmd state

show_history :: Cmd.CmdL String
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
write_state :: FilePath -> Cmd.CmdL ()
write_state = Save.write_state

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

-- * show / modify UI state

show_state :: Cmd.CmdL String
show_state = do
    State.State views blocks tracks rulers _ <- State.get
    let f fm = PPrint.list (map show (Map.keys fm))
    return $ PPrint.record
        [ ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        ]

-- ** views

get_views :: Cmd.CmdL [ViewId]
get_views = State.gets (Map.keys . State.state_views)

-- | Show all views whose view id matches a string.
show_views :: String -> Cmd.CmdL String
show_views match = do
    st <- State.get
    return $ Pretty.formatted $ Util.match_map match (State.state_views st)

-- ** blocks

show_block :: BlockId -> Cmd.CmdL String
show_block block_id = Pretty.formatted <$> State.get_block block_id

-- | Show all blocks whose block id matches a string.
-- Useful for quick block inspection.
show_blocks :: String -> Cmd.CmdL String
show_blocks match = do
    st <- State.get
    return $ Pretty.formatted $ Util.match_map match (State.state_blocks st)

-- ** tracks

show_track :: TrackId -> Cmd.CmdL Simple.Track
show_track = Simple.dump_track

-- ** misc

collapse_track, expand_track :: BlockId -> TrackNum -> Cmd.CmdL ()
collapse_track block_id tracknum = do
    -- TODO if the track to collapse is a pitch track, merge it with its
    -- note track instead
    State.add_track_flag block_id tracknum Block.Collapse
    Info.set_inst_status block_id tracknum
expand_track block_id tracknum = do
    State.remove_track_flag block_id tracknum Block.Collapse
    Info.set_inst_status block_id tracknum

-- | Called from logview.
collapse, expand :: TrackNum -> Cmd.CmdL ()
collapse tracknum = flip collapse_track tracknum =<< Cmd.get_focused_block
expand tracknum = flip expand_track tracknum =<< Cmd.get_focused_block


-- Modify global keymap

-- | Called from the browser.
load_instrument :: Text -> Cmd.CmdL ()
load_instrument = LInst.load
