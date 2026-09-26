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
{-# LANGUAGE UndecidableInstances #-}
module Cmd.Repl.Global (
    module Cmd.Repl.Global, module Cmd.ModifyEvents
) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
-- Just make sure these are compiled.
-- These are used to write patterns for 'ModifyEvents.substitute'.
import qualified App.ReplProtocol as ReplProtocol
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Info as Info
import           Cmd.ModifyEvents (Replacement(F), w, ws, ws1)
import           Cmd.Repl.LEvent ()
import           Cmd.Repl.LInst ()
import           Cmd.Repl.LPerf ()
import           Cmd.Repl.LPitch ()
import           Cmd.Repl.LRuler ()
import           Cmd.Repl.LTrack ()
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection

import qualified Derive.Stack as Stack
import           Midi.Synth ()
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | Take a string and automatically figure out what kind of ID is expected and
-- add a namespace if one was not already in the string.
--
-- Throws an error if the ID has bad characters, which is ok since this is
-- expected to be used from the REPL.
--
-- This is used by the REPL's macro feature, to replace @xyz@ with
-- (make_id "current-namespace" "xyz")
makeId :: Id.Ident a => Text -> Text -> a
makeId ns name =
    fromMaybe (error $ "invalid characters in id: " ++ show name) $
        Id.make $ Id.readShort (Id.namespace ns) name

vid :: Text -> ViewId
vid = Id.ViewId . Id.readId

bid :: Text -> BlockId
bid = Id.BlockId . Id.readId

rid :: Text -> RulerId
rid = Id.RulerId . Id.readId

tid :: Text -> TrackId
tid = Id.TrackId . Id.readId

-- | Get the current focused block.
block :: Cmd.CmdL BlockId
block = Cmd.getFocusedBlock

-- | Get the track under the selection.
track :: Cmd.CmdL TrackId
track = do
    (_, _, trackId, _) <- Selection.getInsert
    pure trackId

tracknum :: Cmd.CmdL TrackNum
tracknum = do
    (_, tracknum, _, _) <- Selection.getInsert
    pure tracknum

-- | Get the current focused view.
view :: Cmd.CmdL ViewId
view = Cmd.getFocusedView

-- | RulerId of the ruler under the selection.
ruler :: Cmd.CmdL RulerId
ruler = do
    n <- tracknum
    blockId <- block
    Cmd.abortUnless =<< Ui.rulerTrackAt blockId n

-- | Get the root block.
root :: Cmd.CmdL BlockId
root = Ui.getRootId

-- | Create a namespace, and throw an IO exception if it has bad characters.
-- Intended to be used from the REPL, where throwing an IO exception is ok.
ns :: Text -> Id.Namespace
ns name
    | Id.validSymbol name = Id.namespace name
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
pp :: Pretty a => Cmd.CmdL a -> Cmd.CmdL Text
pp = fmap Pretty.formatted

quit :: Cmd.CmdL ()
quit = Cmd.modify $ \st -> st { Cmd.stateReplStatus = Cmd.Quit }

-- * errors

-- | Called from logview
s :: String -> Cmd.CmdL ()
s stackpos = maybe (Cmd.throw $ "can't parse stackpos: " <> showt stackpos)
    highlightError (Stack.parseUiFrame stackpos)

-- | I used to use 'Config.error_selnum' for this, but it could be hard to
-- find.  Maybe the normal selection is more convenient, since I can zoom in on
-- it, or move it around to find it.
highlightError :: Stack.UiFrame -> Cmd.CmdL ()
highlightError (maybeBid, maybeTid, maybeRange) = do
    blockId <- maybe findBlock pure maybeBid
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    viewIds <- if null viewIds then (:[]) <$> Create.view blockId
        else pure viewIds
    mapM_ Cmd.focus viewIds
    case (maybeTid, maybeRange) of
        (Nothing, _) -> forM_ viewIds $ \vid ->
            Selection.setAndScroll vid $ Sel.Selection
                { startTrack = 0, startPos = 0
                , curTrack = 9999, curPos =  9999
                , orientation = Sel.None
                }
        (Just tid, Nothing) -> do
            tracknum <- Ui.getTracknumOf blockId tid
            forM_ viewIds $ \vid ->
                Selection.setAndScroll vid $ Sel.Selection
                    { startTrack = tracknum, startPos = 0
                    , curTrack = tracknum, curPos = 9999
                    , orientation = Sel.None
                    }
        (Just tid, Just (from, to)) -> do
            tracknum <- Ui.getTracknumOf blockId tid
            forM_ viewIds $ \vid -> do
                Selection.setAndScroll vid $ Sel.Selection
                    { startTrack = tracknum, startPos = to
                    , curTrack = tracknum, curPos = from
                    , orientation = Sel.None
                    }
    where
    findBlock = case maybeTid of
        Nothing -> Cmd.throw $
            "can't highlight stack frame with neither block nor track: "
            <> showt (maybeBid, maybeTid, maybeRange)
        Just trackId -> maybe (Cmd.throw $ "no block with " <> showt trackId)
            (pure . fst) . Lists.head =<< Ui.blocksWithTrackId trackId

-- * show / modify cmd state

showHistory :: Cmd.CmdL Text
showHistory = do
    hist <- Cmd.gets Cmd.stateHistory
    pure $ Pretty.formatted hist

-- * load / save

save :: Cmd.CmdL ()
save = Save.save

saveAs :: FilePath -> Cmd.CmdL ()
saveAs = Save.saveAs

-- | Save to the default filename and switch to saving plain states.
saveState :: Cmd.CmdL ()
saveState = Save.saveState

-- | Save to the given filename and switch to saving plain states.
saveStateAs :: FilePath -> Cmd.CmdL ()
saveStateAs = Save.saveStateAs

-- | Like 'save_state', but don't change 'Cmd.state_save_file'.
writeState :: FilePath -> Cmd.CmdL FilePath
writeState = Save.writeCurrentState

saveGit :: Cmd.CmdL ()
saveGit = Save.saveGit

-- | Save to the given git repo and switch to saving incrementally.
saveGitAs :: FilePath -> Cmd.CmdL ()
saveGitAs = Save.saveGitAs

load :: FilePath -> Cmd.CmdL ()
load = Save.load

loadForce :: FilePath -> Cmd.CmdL ()
loadForce = Save.loadForce

revert :: Cmd.CmdL ()
revert = Save.revert Nothing

revertTo :: String -> Cmd.CmdL ()
revertTo = Save.revert . Just

-- * called externally

collapseTrack, expandTrack :: BlockId -> TrackNum -> Cmd.CmdL ()
collapseTrack blockId tracknum = do
    -- TODO if the track to collapse is a pitch track, merge it with its
    -- note track instead
    Ui.addTrackFlag blockId tracknum Block.Collapse
    Info.setInstrumentStatus blockId tracknum
expandTrack blockId tracknum = do
    Ui.removeTrackFlag blockId tracknum Block.Collapse
    Info.setInstrumentStatus blockId tracknum

-- | Called from logview.
collapse, expand :: TrackNum -> Cmd.CmdL ()
collapse tracknum = flip collapseTrack tracknum =<< Cmd.getFocusedBlock
expand tracknum = flip expandTrack tracknum =<< Cmd.getFocusedBlock

-- | The result of a REPL Cmd is converted to a 'ReplProtocol.Result' with
-- this method.
class Return a where
    _toResult :: a -> ReplProtocol.Result
instance {-# OVERLAPPABLE #-} Show a => Return a where
    _toResult = ReplProtocol.Format . showt
instance Return ReplProtocol.Result where
    _toResult = id
instance Return Text where
    _toResult = ReplProtocol.Raw
instance Return String where
    _toResult = ReplProtocol.Raw . txt
instance Return () where
    _toResult () = ReplProtocol.Raw ""
