module App.LoadConfig where
import Control.Monad
import qualified Util.Log as Log
import qualified Ui.Style as Style
import qualified Ui.StyleC as StyleC
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC


-- | Tell the UI layer about the given Symbols.  Warnings are logged for
-- Symbols that couldn't be loaded.
symbols :: [Symbol.Symbol] -> IO ()
symbols syms = do
    forM_ syms $ \sym -> do
        missing <- SymbolC.insert_symbol sym
        when (not (null missing)) $
            Log.warn $ "failed to load symbol " ++ show (Symbol.sym_name sym)
                ++ ", fonts not found: " ++ show missing
    Log.notice $ "loaded " ++ show (length syms) ++ " symbols"

styles :: [Style.Style] -> IO ()
styles style_table =
    sequence_ [StyleC.insert_style (Style.StyleId n) style
        | (n, style) <- zip [0..] style_table]
