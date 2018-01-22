-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2018.
module Solkattu.Score.Solkattu2018 where
import Prelude hiding ((.), (^), repeat)

import Solkattu.SolkattuGlobal


yt_mannargudi1 :: Korvai
yt_mannargudi1 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 4, 05), (0, 5, 22))) $
        eddupu (3/2/2) $
        korvai adi mridangam $ map su
    [ sarvaM 8 . theme.din.__4 . group3 (1^theme) . theme.din.__8 . sarvaD 4
        . theme.din.__4 . group3 (1^theme) . din.__6
        . su tarikita.theme.tat.__6
        . su (kita.tarikita.dit.__) . group3 theme . din.__2
        . restD 1 . su tarikita.theme.din.__2
        . su (kita.tarikita.dit.__) . group3 theme
        -- Korvai starts here, but it starts on arudi.
        -- TODO so I need arbitrary section boundaries.
        . repeat 3 (theme . spread 3 tdgnt . theme . spread 2 tdgnt . theme
            . tri_ __3 (tri p5))
            -- Reduce __3 karvai in utarangam to __2 to arrive on sam.
    ]
    where
    theme = takita.ta.takadinna
    group3 (a:b:cs) = [a] . [b] . __ . group3 cs
    group3 xs = xs
    mridangam = makeMridangam
        [ (takita.ta, [k, k, t, k])
        , (1^takita.ta, [p&k, p&k, t, k])
        , (din, [od])
        , (tat, [k])
        , (tarikita, [o, k, n, p])
        , (kita.tarikita.dit, [k, t, p, k, p, t, p])
        ]
