-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.Mridangam2016 where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal


t_16_11_14 :: Korvai
t_16_11_14 = date 2016 11 14 $ exercise $ ganesh $ korvai1 adi $ nadai 6 $
    tri (reduce3 1 ø (o&n.p.k.o&d.__)) . o&n.p.k . o&n.p.k.__ . o&n.p.k.__3
