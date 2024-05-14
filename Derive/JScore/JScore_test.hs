-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.JScore.JScore_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Cmd.Integrate.Convert as Convert
import qualified Derive.JScore.JScore as JScore
import qualified Ui.Event as Event

import           Global
import           Types
import           Util.Test.Global


test_convert_source :: Test
test_convert_source = do
    let header =
            "%laras = slendro-manyura\n\
            \%instrument = gender-barung\n\
            \%irama = tanggung\n"
    let f = fmap fst . convert_source . (header <>)
    let events es =
            [ (t, -0, "-- " <> Text.singleton e)
            | (t, e) <- zip (Lists.range_ 1 1) es
            ]
    right_equal (f
        "3231 dualolo [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n"
        )
        [ (">", events "3231")
        , (">gb", [(0, 4, "gb-t-dll")])
        ]
    right_equal (f
        "3231 dualolo [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5321 dualolo\n"
        )
        [ (">", events "32315321")
        , (">gb", [(0, 4, "gb-t-dll"), (4, 4, "gb-t-dll")])
        ]
    left_like (f
        "3231 dualolo [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5321 tumurun\n"
        )
        "empty block gb-t-tm has no matches"
    right_equal (f
        "3211 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5211 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5311 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n"
        )
        [ (">", events "321152115311")
        , (">gb", [(0, 4, "gb-t-tm"), (4, 4, "gb-t-tm"), (8, 4, "gb-t-tm")])
        ]
    -- Same tracks get unified.
    right_equal (f
        "3211 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5211 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \5311 tumurun [\n\
        \    > 6666 | 6666\n\
        \    > 2222 | 2222\n\
        \]\n"
        )
        [ (">", events "321152115311")
        , (">gb",
            [ (0, 4, "gb-t-tm-3211")
            , (4, 4, "gb-t-tm-3211")
            , (8, 4, "gb-t-tm-5311")
            ])
        ]
    right_equal (f
        "1111 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \2222 tumurun [\n\
        \    > 6666 | 6666\n\
        \    > 2222 | 2222\n\
        \]\n\
        \1111 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n"
        )
        [ (">", events "111122221111")
        , (">gb",
            [ (0, 4, "gb-t-tm-1111")
            , (4, 4, "gb-t-tm-2222")
            , (8, 4, "gb-t-tm-1111")
            ])
        ]
    right_equal (f
        "1111 tumurun [\n\
        \    > 5555 | 5555\n\
        \    > 1111 | 1111\n\
        \]\n\
        \1111 tumurun [\n\
        \    > 6666 | 6666\n\
        \    > 1111 | 1111\n\
        \]\n"
        )
        [ (">", events "11111111")
        , (">gb",
            [ (0, 4, "gb-t-tm-1111-1")
            , (4, 4, "gb-t-tm-1111-2")
            ])
        ]

    -- Different instruments.
    right_equal (f
        "3231 dualolo [\n\
        \    > 5653 | .6.56 1\n\
        \    > .12_ | 6 2 321\n\
        \]\n\
        \%instrument = gender-panerus\n\
        \3231 [\n\
        \    > '161..616 | ..616.61\n\
        \    >  ...35... | 35...5..\n\
        \]\n"
        )
        [ (">", events "3231")
        , (">gb", [(0, 4, "gb-t-dll")])
        , (">gp", [(0, 4, "gp-t-seleh-1")])
        ]
    left_like (f
        "3231 dualolo [\n\
        \    > 5653 | .6.56 1\n\
        \    > .12_ | 6 2 321\n\
        \]\n\
        \%instrument = gender-panerus\n\
        \5231 [\n\
        \    > '161..616 | ..616.61\n\
        \    >  ...35... | 35...5..\n\
        \]\n"
        )
        "*inconsistent gatra:\n3231 5231*"

type Track = (Text, [Event])
type Event = (TrackTime, TrackTime, Text)

convert_source :: Text -> Either Text ([Track], [[Track]])
convert_source = bimap (Text.intercalate "; ") extract . JScore.convert_source
    where
    extract (top, bs) = (e_block top, map e_block bs)
    e_block = map (e_track . fst) . JScore.block_tracks
    e_track track =
        (Convert.track_title track, map e_event (Convert.track_events track))
    e_event e = (Event.start e, Event.duration e, Event.text e)

test_collect_columns :: Test
test_collect_columns = do
    let f = Map.toList . fmap (map snd) . JScore.collect_columns fst
    equal (f
        [ [(1, 'a'), (2, 'b')]
        , [(2, 'c')]
        , [(1, 'd')]
        ])
        [(1, "ad"), (2, "bc")]
