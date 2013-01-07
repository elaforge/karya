{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lang cmds providing general UI state operations.
module Cmd.Lang.LState where
import qualified Data.Time as Time

import Util.Control
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


-- * configure

get_config :: Cmd.CmdL State.Config
get_config = State.config <#> State.get

get_default :: Cmd.CmdL State.Default
get_default = State.config#State.default_ <#> State.get

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

set_global_transform :: String -> Cmd.CmdL ()
set_global_transform = State.modify . (State.config#State.global_transform #=)

get_global_transform :: Cmd.CmdL String
get_global_transform = State.config#State.global_transform <#> State.get

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

rename :: String -> Cmd.CmdL ()
rename ns = case Id.namespace ns of
    Nothing -> Cmd.throw $ "invalid namespace: " ++ show ns
    Just ns -> Create.rename_project ns
