-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Derive.Solkattu.MridangamScore where
import Prelude hiding ((.), repeat)

import qualified Util.CallStack as CallStack
import Derive.Solkattu.Dsl hiding ((&))
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.MridangamDsl
import qualified Derive.Solkattu.Solkattu as Solkattu


sriram :: [Korvai]
sriram = korvais (adi 4)
    [ o&n.__.p.k.t.k . repeat 3 (n.k.t.p.k.t.k)
        . repeat 4 (o.t.k.n . s2 (k.t.p.k))
        . repeat 4 (n . s2 (k.t.p.k)  . o.t.k)
        . o&n.__.p.k . tari . repeat 3 (p.k.p.t.p.k . tari)
    ]

janahan :: [Korvai]
janahan = korvais (adi 4)
    [ o&d.__ . repeat 4 (n.p.k.t.p.k.t.p) . k.t.p.k.o&d
    ]

tari :: Sequence
tari = n.p.u.p.k.t.p.k

-- * realize

adi :: Matras -> Solkattu.Tala
adi = Solkattu.adi_tala

korvais :: CallStack.Stack => Solkattu.Tala -> [Sequence] -> [Korvai]
korvais tala = map (korvai tala)

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_ True
realizep = realize_ False

realize_ :: Bool -> Korvai.Korvai -> IO ()
realize_ = realize_instrument Korvai.mridangam
