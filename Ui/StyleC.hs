-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.StyleC (insert_style) where
import ForeignC

import qualified Ui.Style as Style


insert_style :: Style.StyleId -> Style.Style -> IO ()
insert_style (Style.StyleId style_id) style =
    with style $ \stylep -> c_insert_style (fromIntegral style_id) stylep

foreign import ccall "insert_style"
    c_insert_style :: CUChar -> Ptr Style.Style -> IO ()
