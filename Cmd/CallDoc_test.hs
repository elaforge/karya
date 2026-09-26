-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.CallDoc_test where
import Util.Test
import qualified Cmd.CallDoc as CallDoc
import qualified Derive.C.All as C.All
import qualified Derive.Scale.All as Scale.All


test_doc_html :: Test
test_doc_html = do
    -- Mostly this just makes HPC coverage for all the documentation.
    let hstate = ("haddock", mempty)
    notEqual (CallDoc.docHtml hstate (CallDoc.builtins C.All.builtins))
        mempty
    notEqual (CallDoc.scalesHtml hstate (CallDoc.scaleDocs Scale.All.docs))
        mempty
