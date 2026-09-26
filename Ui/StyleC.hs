-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.StyleC (insertStyle) where
import ForeignC

import qualified Ui.Style as Style


insertStyle :: Style.StyleId -> Style.Style -> IO ()
insertStyle (Style.StyleId styleId) style =
    with style $ \stylep -> cInsertStyle (fromIntegral styleId) stylep

foreign import ccall "insert_style"
    cInsertStyle :: CUChar -> Ptr Style.Style -> IO ()
