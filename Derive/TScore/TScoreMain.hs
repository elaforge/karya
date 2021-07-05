-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone driver for tscore.
module Derive.TScore.TScoreMain where
import qualified Control.Monad.Except as Except
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified System.Environment as Environment

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ky as Ky
import qualified Cmd.Performance as Performance
import qualified Cmd.Simple as Simple

import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.TScore.T as T
import qualified Derive.TScore.TScore as TScore

import qualified Instrument.Inst as Inst
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global

-- TODO this compiles Cmd.GlobalKeymap, why?
-- SyncKeycaps -> User.Elaforge.Config -> Local.Config -> DeriveSaved
--
-- So I must either split config into interactive and non-interactive, or have
-- some hack to open keycaps without directly calling SyncKeycaps.

main :: IO ()
main = do
    [fname] <- Environment.getArgs
    source <- Text.IO.readFile fname
    (ui_state, cmd_state) <- either errorIO return =<< load_score source
    let (events, logs) = derive ui_state cmd_state
    dump <- either (errorIO . pretty) return $
        Ui.eval ui_state $ Simple.dump_state
    Pretty.pprint dump
    -- PPrint.pprint dump
    putStrLn "\nevents:"
    mapM_ Log.write logs
    -- mapM_ Pretty.pprint events
    mapM_ (Text.IO.putStrLn . Score.short_event) events
    let (midi, midi_logs) = DeriveSaved.perform_midi cmd_state ui_state events
    putStrLn "\nmidi:"
    mapM_ Log.write midi_logs
    mapM_ Pretty.pprint midi

derive :: Ui.State -> Cmd.State -> (Vector.Vector Score.Event, [Log.Msg])
derive ui_state cmd_state = case Ui.config#UiConfig.root #$ ui_state of
    Nothing -> (mempty, [Log.msg Log.Error Nothing "no root block"])
    Just block_id -> (Cmd.perf_events perf, warns ++ logs)
        where
        (perf, logs) = Performance.derive ui_state cmd_state block_id
        warns = filter ((>=Log.Warn) . Log.msg_priority) (Cmd.perf_logs perf)

type Error = Text

load_score :: Text -> IO (Either Error (Ui.State, Cmd.State))
load_score source = Except.runExceptT $ do
    cmd_config <- liftIO DeriveSaved.load_cmd_config
    (ui_state, instruments) <- tryRight $ TScore.parse_score source
    (builtins, aliases) <- tryRight . first ("parsing %ky: "<>)
        =<< liftIO (Ky.load ky_paths ui_state)
    let cmd_state =  DeriveSaved.add_library builtins aliases $
            Cmd.initial_state cmd_config
    ui_state <- tryRight $ first pretty $ Ui.exec ui_state $
        forM_ instruments $ uncurry (allocate cmd_config) . convert_allocation
    return (ui_state, cmd_state)
    where
    -- For now, I don't support ky import.
    ky_paths = []

-- | Like 'Cmd.allocate', but doesn't require Cmd.M.
allocate :: Ui.M m => Cmd.Config -> ScoreT.Instrument -> UiConfig.Allocation
    -> m ()
allocate cmd_config score_inst alloc = do
    let qualified = UiConfig.alloc_qualified alloc
    inst <- Ui.require ("instrument not in db: " <> pretty qualified) $
        Cmd.state_lookup_qualified cmd_config qualified
    allocs <- Ui.config#UiConfig.allocations <#> Ui.get
    allocs <- Ui.require_right id $
        UiConfig.allocate (Inst.inst_backend inst) score_inst alloc allocs
    Ui.modify_config $ UiConfig.allocations #= allocs

convert_allocation :: T.Allocation -> (ScoreT.Instrument, UiConfig.Allocation)
convert_allocation (T.Allocation inst qual backend) =
    ( ScoreT.Instrument inst
    , UiConfig.allocation qual $ case backend of
        T.Im -> UiConfig.Im
        T.Midi chans -> UiConfig.Midi $
            Midi.Patch.config (map (, Nothing) chans)
    )
