-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.PakhawajScore where
import Data.Text (Text)
import qualified Data.Text as Text


bats :: [Text]
bats =
    [ Text.unlines
    [ "ga di ge ne na ge tet te"
    , "ga di ge ne na ge tet te"
    , "ka ta ka ta ga di ga di"
    , "ga di ge ne na ge tet te"
    --
    , "ta ki tet ta - tet ta"
    , "ta ki tet dhet tet te dha ge"
    , "te re ki ta ta ka ta -"
    , "tetekata"
    ]

    , Text.unlines
    [ "dha ge tet te   | Ta ge tet te    | kre dhet tet te | dha ge tet te"
    , "tet te kre dhet | tet te dha ge   | tet te ka Ta    | ga di ge ne"
    , "ta ki te Ta     | - te Ta -       | ga - di -       | ge re na ge"
    , "te re ki ta     | ta ka Ta -      | tetekata"
    ]

    , Text.unlines
    [ "dha ge tet te   | Ta ge tet te    | dha ge tet te   | Ta ge tet te"
    , "kre dhet tet te | dha ge tet te   | kre dhet tet te | dha ge tet te"
    , "kre dhet tet te | kre dhet tet te | dha ge tet te   | kre dhet tet te"
    , "kre dhet tet te | dha ge tet te   | ga di ge ne     | na ge tet te"
    , "kat tet te kat  | tet te kat Ta   | kat tr kt tk    | Ta ge tet te"
    , "kre dhet - dhet | tet te dha ge   | tet te ka Ta    | ga di ge ne"
    , "dha - ki ta     | ta ka dhu ma    | ki ta ta ka     | dhet - Ta -"
    , "te re ki ta     | ta ka Ta -      | tetekata"
    ]

    , Text.unlines
    [ "ge ge tet te    | ga di ge ne     | na ge te re     | ki ta ta ka"
    , "Ta ge tet te    | ge ge tet te    | ga di ge ne     | na ge tet te"
    , "gre dhin - Ta   | - ne Ta -       | ge ge tet te    | ga di ge ne"
    , "Ta ge tet te    | ge ge tet te    | ga di ge ne     | na ge tet te"
    ]

    , Text.unlines
    [ "dhet te dhet te | dha ge tet te   | kre dhet tet te | dha ge tet te"
    , "kre dhet tet te | kre dhet tet te | kre dhet tet te | dha ge tet te"
    , "kre dhet - Ta   | ge ne dha -     | tetekata"
    , "na ge tet te    | ka Ta ka Ta     | ka tr kt dhet   | tet te ka ta"
    , "ge - te ran     | - ne dha -      | di - ge -       | - - Ta -"
    , "dha - - ne      | ka ta ka ta     | ge - te ran     | - ne dha -"
    , "di - ge -       | - - Ta -        | dha - - ne      | ka ta ka ta"
    , "ge - te ran     | - ne dha -      | di - ge -       | - - Ta -"
    ]

    ]
