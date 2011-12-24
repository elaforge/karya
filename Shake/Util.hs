module Shake.Util (Cmdline, system) where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as FilePath
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit


type Cmdline = (String, String, [String])

system :: Cmdline -> Shake.Action ()
system (abbr, output, cmd_:args) = do
    let cmd = FilePath.toNative cmd_
    let desc = abbr ++ ": " ++ output
    Shake.putLoud desc
    res <- Trans.liftIO $ Cmd.rawSystem cmd args
    when (res /= Exit.ExitSuccess) $
        error $ "Failed:\n" ++ unwords (cmd : args)
system (abbr, output, []) =
    error $ "0 args for system: " ++ show (abbr, output)
