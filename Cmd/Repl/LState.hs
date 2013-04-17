{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Repl cmds providing general UI state operations.
module Cmd.Repl.LState where
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.Posix as Posix

import Util.Control
import qualified Util.File as File
import qualified Util.Pretty as Pretty

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Load.Midi as Load.Midi
import qualified Cmd.Repl.LBlock as LBlock
import qualified Cmd.Repl.LEvent as LEvent
import qualified Cmd.Repl.LTrack as LTrack

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- | Find text in block titles, track titles, or events.
find :: Text -> Cmd.CmdL String
find search = do
    blocks <- LBlock.find search
    tracks <- LTrack.find search
    events <- LEvent.find search
    return $ unlines $ concatMap section $ concat
        [ [("blocks:", Pretty.formatted blocks) | not (null blocks)]
        , [("tracks:", Pretty.formatted tracks) | not (null tracks)]
        , [("events:", Pretty.formatted events) | not (null events)]
        ]
    where section (title, doc) = [title, doc]

-- * configure

get_config :: Cmd.CmdL State.Config
get_config = State.config <#> State.get

get_default :: Cmd.CmdL State.Default
get_default = State.config#State.default_ <#> State.get

get_default_tempo :: Cmd.CmdL Signal.Y
get_default_tempo = State.config#State.default_#State.tempo <#> State.get

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t = modify_config $ State.default_#State.tempo #= t

set_default_inst :: String -> Cmd.CmdL ()
set_default_inst inst = modify_config $
    State.default_#State.instrument #= Just (Score.Instrument inst)

set_default_scale :: String -> Cmd.CmdL ()
set_default_scale scale = modify_config $
    State.default_#State.scale #= Pitch.ScaleId scale

modify_config :: (State.Config -> State.Config) -> Cmd.CmdL ()
modify_config f = State.modify_config f >> Cmd.invalidate_performances

-- | 'State.config_global_transform' is an expression that's applied to the
-- output of derivation.
set_global_transform :: Text -> Cmd.CmdL ()
set_global_transform = State.modify . (State.config#State.global_transform #=)

get_global_transform :: Cmd.CmdL Text
get_global_transform = State.config#State.global_transform <#> State.get

instruments :: Cmd.CmdL (Map.Map Score.Instrument Score.Instrument)
instruments = State.config#State.instruments <#> State.get

add_instrument :: String -> String -> Cmd.CmdL ()
add_instrument from to = State.modify $
    State.config#State.instruments
        %= Map.insert (Score.Instrument from) (Score.Instrument to)

remove_instrument :: String -> Cmd.CmdL ()
remove_instrument inst = State.modify $
    State.config#State.instruments %= Map.delete (Score.Instrument inst)

-- ** meta

get_meta :: Cmd.CmdL State.Meta
get_meta = State.config#State.meta <#> State.get

set_creation_time :: Cmd.CmdL ()
set_creation_time = do
    now <- liftIO Time.getCurrentTime
    State.modify $ State.config#State.meta#State.creation #= now

set_notes :: String -> Cmd.CmdL ()
set_notes = State.modify . (State.config#State.meta#State.notes #=)


-- * transform

-- | Set the score namespace to the given one.  Also update the project_dir
-- and move the actual directory.
rename :: String -> Cmd.CmdL ()
rename ns = do
    ns <- Cmd.require_msg ("invalid namespace: " ++ show ns) (Id.namespace ns)
    Create.rename_project ns
    Cmd.gets Cmd.state_save_file >>= \x -> case x of
        Nothing -> return ()
        Just (Cmd.SaveState fn) -> Cmd.modify $ \st -> st
            { Cmd.state_save_file = Just $ Cmd.SaveState $ replace_dir ns fn }
        Just (Cmd.SaveGit repo) -> do
            -- System.Directory.renameDirectory deletes the destination
            -- diretory for some reason.  I'd rather throw an exception.
            let old_dir = FilePath.takeDirectory repo
                new_dir = FilePath.replaceFileName old_dir (Id.un_namespace ns)
            Cmd.rethrow_io $ File.ignore_enoent $ Posix.rename old_dir new_dir
            Cmd.modify $ \st -> st
                { Cmd.state_save_file = Just $ Cmd.SaveState $
                    new_dir </> FilePath.takeFileName repo
                }
    where
    replace_dir ns path = new_dir </> FilePath.takeFileName path
        where
        new_dir = FilePath.replaceFileName (FilePath.takeDirectory path)
            (Id.un_namespace ns)

-- * load

load_midi :: FilePath -> Cmd.CmdL BlockId
load_midi = Load.Midi.load
