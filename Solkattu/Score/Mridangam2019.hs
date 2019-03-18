-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2019 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


e_naka :: Korvai
e_naka = sarvalaghu $ ganesh $ date 2019 4 14 $ korvai1 adi $ section $
    n.k.o.od.__.k.on.k . o & repeat 3 (n.k.p.d.__.k.n.k)
