module Derive.Call.Val where
import qualified Cmd.TimeStep as TimeStep
import qualified Derive.Args as Args
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required)
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang


val_calls :: Derive.ValCallMap
val_calls = Derive.make_calls
    [ ("t", c_timestep)
    ]

c_timestep :: Derive.ValCall
c_timestep = Derive.val_call "timestep"
    ("Compute the duration of the given RelativeMark timestep at the current\
    \ position. This is for durations, so it only works with RelativeMark, and\
    \ in fact prepends `r:`, so e.g. a quarter note is just `q`."
    ) $ CallSig.call2g
    ( required "timestep"  ("Emit a duration of this timestep.\
        \This must a relative marklist timestep, and `r:` will be prepended to\
        \it.")
    , optional "steps" 1 "Step this number of times, negative to step back."
    ) $ \timestep steps args -> do
        timestep <- Derive.require_right ("parsing timestep: "++) $
            TimeStep.parse_time_step ("r:" ++ timestep)
        (block_id, tracknum) <- Internal.get_current_tracknum
        let start = Args.start args
        end <- Derive.require ("valid timestep from " ++ ShowVal.show_val start)
            =<< Derive.eval_ui "c_timestep"
                (TimeStep.step_from steps timestep block_id tracknum start)
        return $ TrackLang.score_time (end - start)
