-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Korvais expressed in SolkattuDsl.
module Derive.Call.India.SolkattuScore where
import Prelude hiding ((-), repeat)
import qualified Data.Text.IO as Text.IO

import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.SolkattuDsl
import Global


-- * chatusra nadai

chatusram1_2 :: Korvai
chatusram1_2 = check $ Solkattu.korvai (adi 4) chatusram1_mridangam $
      theme 0 - p5
    - dropM 2 (theme 1) - p6 - p6
    - dropM 4 (theme 2) - tri p7 - tri p6 - tri p5
    where
    theme gap = ta - __ - dit - __ - ta - ka - din - na - din
        - tri (ta - __n gap - din - __)

chatusram1_3 :: Korvai
chatusram1_3 = check $ Solkattu.korvai (adi 4) chatusram1_mridangam $
      theme 0 - p5 - tadin
    - dropM 3 (theme 1) - p6 - p6 - tadin
    - dropM 6 (theme 2) - trin tadin (tri p7) (tri p6) (tri p5)
    where
    theme gap = ta - __2 - dit - __2 - ta - ka - din - na - din
        - tri (ta - __n gap - din - __2)
    tadin = ta - __ - din - __ - __

chatusram1_4 :: Korvai
chatusram1_4 = check $ Solkattu.korvai (adi 4) mridangam $
      theme 0 - pat7 - st u dheem - __3
    - dropM 4 (theme 1) - repeat 2 pat8 - st u dheem - __3
    - dropM 8 (theme 2)
        - trin (st i dheem - __3) (tri pat9) (tri pat8) (tri pat7)
    where
    theme gap = ta - __3 - dit - __3 - ta - ka - din - na - din
        - tri (ta - __n gap - din - __3)
    pat7 = ta - ka - p5
    pat8 = ta - ka - __ - p5
    pat9 = ta - __ - ka - __ - p5
    mridangam = chatusram1_mridangam ++
        [ (ta - ka, [k, p])
        ]

chatusram1_mridangam :: [(Sequence, [MNote])]
chatusram1_mridangam =
    [ (ta - dit, [k, t])
    , (dit, [k])
    , (ta - ka - din - na - din, [k, o, o, k, o])
    , (ta - din, [k, od])
    ]

-- * kanda nadai

make_k1 :: Sequence -> Either Text Korvai
make_k1 = Solkattu.korvai (adi 5) k1_mridangam

make_k1_1 :: Sequence -> Sequence -> Korvai
make_k1_1 pt gap = check $ make_k1 $
      at0 - k1_a  - __ - ta - din - __n (subtract pdur 9) - pt
    - atX - k1_a' - __ - ta - din - __n (subtract pdur 9) - pt
    - at0 - ta - __ - di - __ - ki - ta - __ - gap
    -       ta - ka - di - __ - ki - ta - __ - gap
    - case pdur of
        5 -> p567 end_gap
        6 -> p666 end_gap
        _ -> p765 end_gap
    where
    pdur = duration_of pt
    end_gap = __n (subtract (duration_of gap) 4)

k1_1_vars :: [Korvai]
k1_1_vars = [make_k1_1 p g | g <- gaps, p <- [pat 5, pat 6, pat 7]]
    where gaps = [thom - __ - ta - __, thom - __2, thom - __, thom, mempty]

k1_2 :: Korvai
k1_2 = check $ make_k1 $
      at0 - k1_a  - __ - ta_din_ - p7
    - atX - k1_a' - __ - ta_din_ - p7
    - at0 - k1_a - __ - k1_a' - __
          - ta_din_ - tri p7
          - ta_din_ - tri (tadin - p7)
          - ta_din_ - tri (tadin - tadin - p7)
    where
    p7 = ta - ka - p5
    ta_din_ = ta - __ - din - __
    tadin = ta - din - __

k1_3 :: Korvai
k1_3 = check $ make_k1 $
      k1_a  - __ - tata_dindin_ - p6 - __
    - k1_a' - __ - tata_dindin_ - ta-ka-p6 - __
    - k1_a - __ - k1_a' - __ - tata_dindin_
    - tri_ __ (ta-ka-ti-ku-p6)
    - tri_ __ (ta-ka-p6)
    - tri_ __ p6
    where
    tata_dindin_ = ta - __ - ta - __2 - din - __ - din - __2

k1_a, k1_a' :: Sequence
k1_a  = ta - __ - di - __ - ki - ta - __ - thom
k1_a' = ta - ka - di - __ - ki - ta - __ - thom

k1_mridangam :: [(Sequence, [MNote])]
k1_mridangam =
    [ (ta, [k])
    , (ta-ka, [k, p])
    , (ta-ka-ti-ku, [k, p, n, p])
    , (ta-di-ki- ta, [k, t, k, n])
    , (ta-ka-di-ki-ta, [k, p, t, k, n])
    , (din, [od])
    ]

adi :: Matras -> Solkattu.Tala
adi = Solkattu.adi_tala

realize :: Solkattu.Korvai -> IO ()
realize korvai = Text.IO.putStrLn $ case result of
    Left err -> "ERROR:\n" <> err
    Right notes -> Solkattu.pretty_strokes_tala tala notes
    where
    result = Solkattu.realize_korvai default_patterns korvai
    tala = Solkattu.korvai_tala korvai
