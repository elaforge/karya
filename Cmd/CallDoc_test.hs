-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.CallDoc_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Cmd.CallDoc as CallDoc
import qualified Derive.Call.All as Call.All
import qualified Derive.Scale.All as Scale.All


test_doc_html = do
    -- Mostly this just makes HPC coverage for all the documentation.
    let hstate = ("haddock", mempty)
    check $ CallDoc.doc_html hstate (CallDoc.all_sections Call.All.scopes)
        /= mempty
    check $ CallDoc.scales_html hstate
            (CallDoc.scale_docs (Map.elems Scale.All.scales))
        /= mempty
