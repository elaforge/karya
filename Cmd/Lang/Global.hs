{- | Helper functions to be imported into Cmd.Lang.Environ.  Cmd.Lang.Environ
    must be interpreted since it's the \"top level\" module, so I put the
    library of commands in here.  An unqualified import in Cmd.Lang.Environ
    means this module is in scope at the REPL.

    Of course, lang commands can use anything in scope in Cmd.Lang.Environ, not
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
    more specialized can go in Cmd.Lang.L* modules.
-}
module Cmd.Lang.Global where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty

-- Just make sure these are compiled.
import Midi.Synth ()
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import Cmd.Lang.LEvent ()
import qualified Cmd.Lang.LInst as LInst
import Cmd.Lang.LPerf ()
import Cmd.Lang.LPitch ()
import Cmd.Lang.LRuler ()
import Cmd.Lang.LTrack ()
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Selection as Selection
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
    Just ns -> case Id.make ns name of
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
infixl 0 $>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 9 .>

-- | Pretty-print the result of a cmd with 'Pretty.format'.
pp :: (Pretty.Pretty a) => Cmd.CmdL a -> Cmd.CmdL String
pp = fmap Pretty.formatted

-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s "" = unerror
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " ++ show stackpos)
        highlight_error (Stack.parse_ui_frame stackpos)

highlight_error :: Stack.UiFrame -> Cmd.CmdL ()
highlight_error (bid, maybe_tid, maybe_range) = do
    unerror
    view_ids <- fmap Map.keys (State.views_of bid)
    mapM_ ViewConfig.bring_to_front view_ids
    case (maybe_tid, maybe_range) of
        (Nothing, _) -> forM_ view_ids $ \vid ->
            Selection.set_selnum vid Config.error_selnum
                (Just (Types.selection 0 0 9999 9999))
        (Just tid, Nothing) -> do
            tracknum <- State.get_tracknum_of bid tid
            forM_ view_ids $ \vid ->
                Selection.set_selnum vid Config.error_selnum
                    (Just (Types.selection tracknum 0 tracknum 9999))
        (Just tid, Just (from, to)) -> do
            tracknum <- State.get_tracknum_of bid tid
            forM_ view_ids $ \vid ->
                Selection.set_and_scroll vid Config.error_selnum
                    (Types.selection tracknum to tracknum from)

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
save = Save.cmd_save =<< Save.get_save_file

save_as :: FilePath -> Cmd.CmdL ()
save_as = Save.cmd_save

load :: FilePath -> Cmd.CmdL ()
load fn
    | SaveGit.is_git fn = Save.cmd_load_git fn Nothing
    | otherwise = Save.cmd_load fn

revert :: Cmd.CmdL ()
revert = Save.cmd_revert Nothing

revert_to :: String -> Cmd.CmdL ()
revert_to = Save.cmd_revert . Just

-- * show / modify UI state

show_state :: Cmd.CmdL String
show_state = do
    (State.State views blocks tracks rulers
        (State.Config ns dir _meta root _midi transform
            (State.Default scale key inst tempo))) <- State.get
    -- midi config showed by show_midi_config
    let f fm = PPrint.list (map show (Map.keys fm))
    return $ PPrint.record
        [ ("namespace", Pretty.pretty ns), ("dir", dir) , ("root", show root)
        , ("global_transform", Pretty.pretty transform)
        , ("views", f views), ("blocks", f blocks)
        , ("tracks", f tracks), ("rulers", f rulers)
        , ("scale", show scale)
        , ("key", show key)
        , ("inst", show inst)
        , ("tempo", show tempo)
        ]

-- ** views

get_views :: Cmd.CmdL [ViewId]
get_views = State.gets (Map.keys . State.state_views)

-- | Show all views whose view id matches a string.
show_views :: String -> Cmd.CmdL String
show_views match = do
    st <- State.get
    return $ Pretty.formatted $
        Map.filterWithKey (\k _ -> match_id match k) (State.state_views st)

-- ** blocks

show_block :: BlockId -> Cmd.CmdL String
show_block block_id = Pretty.formatted <$> State.get_block block_id

-- | Show all blocks whose block id matches a string.
-- Useful for quick block inspection.
show_blocks :: String -> Cmd.CmdL String
show_blocks match = do
    st <- State.get
    return $ Pretty.formatted $
        Map.filterWithKey (\k _ -> match_id match k) (State.state_blocks st)

-- | True if the ID contains the given substring.
match_id :: (Id.Ident id) => String -> id -> Bool
match_id sub = (sub `List.isInfixOf`) . Id.show_id . Id.unpack_id

-- | Tracks that don't appear in any block.
orphan_tracks :: Cmd.CmdL [TrackId]
orphan_tracks =
    fmap (\trefs -> [tid | (tid, refs) <- trefs, refs == 0]) track_refs

track_refs :: Cmd.CmdL [(TrackId, Int)]
track_refs = do
    st <- State.get
    let tids = concatMap Block.block_track_ids
            (Map.elems (State.state_blocks st))
        insert fm tid = Map.insertWith (+) tid 1 fm
        ref_map = List.foldl' insert Map.empty tids
    return [(tid, Map.get 0 tid ref_map)
        | tid <- Map.keys (State.state_tracks st)]

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


-- ** tracks

-- | Some helpers to make it easier to make TracklikeIds.
track :: String -> String -> Block.TracklikeId
track track_id ruler_id = Block.TId (tid track_id) (rid ruler_id)
ruler :: String -> Block.TracklikeId
ruler ruler_id = Block.RId (rid ruler_id)
divider :: Color.Color -> Block.TracklikeId
divider color = Block.DId (Block.Divider color)

-- | Insert a track that already exists.
insert_track :: TrackId -> TrackNum -> Cmd.CmdL ()
insert_track track_id tracknum = do
    block_id <- Cmd.get_focused_block
    ruler_id <- State.ruler_track_at block_id tracknum
    State.insert_track block_id tracknum
        (Block.track (Block.TId track_id ruler_id) Config.track_width)

-- Modify global keymap

-- | Called from the browser.
load_instrument :: String -> Cmd.CmdL ()
load_instrument = LInst.load
