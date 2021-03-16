-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mohra where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Korvai as Korvai

import           Solkattu.Dsl.Generic


korvai :: ([Korvai.Section (SequenceT sollu)] -> Korvai)
    -> (a -> SequenceT sollu) -> (a, a, a) -> (a, a, a) -> Korvai
korvai makeKorvai transform as bs = korvais makeKorvai transform [(as, bs)]

korvais :: ([Korvai.Section (SequenceT sollu)] -> Korvai)
    -> (a -> SequenceT sollu) -> [((a, a, a), (a, a, a))] -> Korvai
korvais makeKorvai transform =
    mohra • makeKorvai • map (section • uncurry (make transform))

-- | Make a mohra in the standard structure.
make :: (a -> SequenceT sollu) -> (a, a, a) -> (a, a, a) -> SequenceT sollu
make transform (a1_, a2_, a3_) (b1_, b2_, b3_) =
      a123.b1 . a123.b1
    . a123.b2
    . a1.b2 . a3.b3
    where
    a123 = a1.a2.a3
    (a1, a2, a3) = (t a1_, t a2_, t a3_)
    (b1, b2, b3) = (t b1_, t b2_, t b3_)
    t = group • transform

-- | Make a 2 speed mohra.
make2 :: (a -> SequenceT sollu) -> ((a, a, a), (a, a, a)) -> SequenceT sollu
make2 transform ((a1_, a2_, a3_), (b1_, b2_, b3_)) =
    a123.b1 . su (a123.b1) . a123.b1 . su (a123.b1)
    . a123.b2 . su (a123.b2)
    . a1.b2 . su (a1.b2)
    . a3.b3 . su (a3.b3)
    where
    (a1, a2, a3) = (t a1_, t a2_, t a3_)
    (b1, b2, b3) = (t b1_, t b2_, t b3_)
    a123 = a1.a2.a3
    t = group • transform
