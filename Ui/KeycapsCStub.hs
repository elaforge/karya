-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Stubbed out KeycapsC, for the same reason as "Ui.BlockCStub".
module Ui.KeycapsCStub (create, destroy, update) where
import qualified Ui.Fltk as Fltk
import qualified Ui.KeycapsT as KeycapsT


create :: (Int, Int) -> KeycapsT.Layout -> Fltk.Fltk ()
create _ _ = nop

destroy :: Fltk.Fltk ()
destroy = nop

update :: KeycapsT.RawBindings -> Fltk.Fltk ()
update _ = nop

nop :: Fltk.Fltk ()
nop = Fltk.fltk (return ())
