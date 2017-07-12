-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Score.Mridangam2017 where
import Prelude hiding ((.), repeat)

import Derive.Solkattu.MridangamGlobal


c_17_07_10 :: Korvai
c_17_07_10 = exercise $ korvai1 adi $ su $
    o.__.k.o.k.o.o.k . repeat 4 (o.o.k.o.k.o.o.k) . repeat 4 (o.k.k.o.o.k)
