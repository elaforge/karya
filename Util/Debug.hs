module Util.Debug where
import qualified Control.Monad.Trans as Trans

import qualified Debug.Trace as Trace

import qualified Midi.CoreMidi as CoreMidi


trace = Trace.trace
tracev x = Trace.trace ("**trace: " ++ show x) x
tracem msg x = Trace.trace ("**" ++ msg ++ ": " ++ show x) x

timer :: (Trans.MonadIO m) => String -> m ()
timer msg = Trans.liftIO $ do
    now <- CoreMidi.now
    putStrLn $ show now ++ ": " ++ msg
