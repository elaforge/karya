-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | One-time startup initialization tasks.
module App.LoadConfig where
import qualified Util.Log as Log
import qualified Ui.Style as Style
import qualified Ui.StyleC as StyleC
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC

import Global


-- | Tell the UI layer about the given Symbols.  Warnings are logged for
-- Symbols that couldn't be loaded.
symbols :: [Symbol.Symbol] -> IO ()
symbols syms = forM_ syms $ \sym -> do
    missing <- SymbolC.insert sym
    unless (null missing) $
        Log.warn $ "failed to load symbol " <> showt (Symbol.name sym)
            <> ", fonts not found: " <> showt missing

styles :: [Style.Style] -> IO ()
styles style_table = sequence_
    [ StyleC.insert_style (Style.StyleId n) style
    | (n, style) <- zip [0..] style_table
    ]
