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

-- * korvais

p16_12_06_sriram2 :: Korvai
p16_12_06_sriram2 = date 2016 12 6 $ sriram $ korvai1 adi $ nadai 7 $
      repeat 2 kook . od.__.k.d.__.k.__                                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__                .od.__7
    . repeat 2 kook . od.__.k.d.__.k.__.n.p.k.d.__.k.__.o.o.k.d.__.k.__.p&u.__7
    . tri (p5.u.__ . p5.u.__.u.__ . p5)
    where kook = k.o.o.k.n.p.k

p16_12_06_janahan1 :: Korvai
p16_12_06_janahan1 = date 2016 12 6 $ janahan $ korvai1 adi $ su $
    tri (op_od_ . on.k.op_od_ . on.k.o&t.k.op_od_)
        . trin __ (tri p5) (tri p6) (tri p7)
    where
    op_od_ = on.p.k.od.__.o

p16_12_06_janahan2 :: Korvai
p16_12_06_janahan2 = date 2016 12 6 $ janahan $ korvai1 adi $ su $
    tri (k.__.t.__.kook) . tri (t.__.kook) . tri kook
        . spread 2 tdgnt . p6
        . spread 2 tdgnt . p6 . tk.p6
        . spread 2 tdgnt . p6 . k.p.p6 . tktu.p6
    where kook = k.o.o.k
