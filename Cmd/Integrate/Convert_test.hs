-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Convert_test where
import qualified Data.Map as Map

import qualified Util.Texts as Texts
import qualified Cmd.Integrate.Convert as Convert
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt as Kontakt

import           Global
import           Util.Test


test_convert :: Test
test_convert = do
    let no_dur = first $ map $ second $ map $ \(s, _, t) -> (s, t)
    let derive0 ptitle = no_dur . derive ptitle
    equal (derive ("kontakt/", "") [(0, 1, "d .25 | -- 4c")])
        ([(">i", [(0.25, 1, "")]), ("*", [(0.25, 0, "4c")])], [])
    equal (derive ("kontakt/", "") [(0, 4, "tr 1d 1 -- 4c")])
        ( [ (">i", map (, 1, "") [0, 1, 2, 3])
          , ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4c"), (3, 0, "4d")])
          ]
        , []
        )
    equal (derive ("kontakt/sc-pemade", "") [(0, 1, "4c"), (1, 0, "+mute --")])
        ( [ (">i", [(0, 1, ""), (1, 0, "+mute")])
          , ("*", [(0, 0, "4c"), (1, 0, "4c")])
          ]
        , []
        )
    equal (derive0 ("kontakt/sc-pemade", "") [(0, 0, ""), (1, 0, "+attr | --")])
        ( [ (">i", [(0, "+mute"), (1, "+attr+mute")])]
        , []
        )
    equal (derive0 ("kontakt/sc-gong", "") [(0, 0, "o --"), (1, 0, "m --")])
        ( [(">i", [(0, "o"), (1, "m")])]
        , []
        )

    let mridangam = derive0
            ("kontakt/mridangam-d", "#=(natural) | import india.mridangam")
    equal (mridangam [(0, 0, "n --"), (1, 0, "d --")])
        ( [(">i", [(0, "n"), (1, "d")])]
        , []
        )
    equal (mridangam [(7, -7, "tir k d_ --")])
        ( [(">i", [(0, "k"), (1, "d"), (3, "k"), (4, "d"), (6, "k"), (7, "d")])]
        , []
        )

    let reyong = derive0 ("kontakt/reyong", "import bali.reyong | voices=1")
    equal (reyong [(0, 0, "+ --"), (1, 0, "XX --")])
        ( [ (">i | v=1 | hand=l", [(0, "+mute"), (1, "+cek")])
          , ("*", [(0, "4e"), (1, "4f")])
          , (">i | v=1 | hand=r", [(0, "+mute"), (1, "+cek")])
          , ("*", [(0, "4g"), (1, "4f")])
          ]
        , []
        )
    equal (reyong [(0, 8, "k -- 4c")])
        ( [ (">i | v=1", map (, "") [0, 1, 2, 4, 5, 6, 8])
          , ( "*"
            , zip [0, 1, 2, 4, 5, 6, 8]
                ["4f", "4e", "4f", "4e", "4f", "4e", "4f"]
            )
          ]
        , []
        )

derive :: (Text, Convert.Title) -> [UiTest.EventSpec]
    -> ([(Convert.Title, [UiTest.EventSpec])], [Text])
derive (patch, title) pitches =
    (map extract (concatMap flatten converted), logs0 ++ logs ++ errors)
    where
    inst = "i"
    allocs = [(inst, patch)]
    result = -- Debug.tracefp "e" Derive.r_events $
        DeriveTest.derive_tracks_setup (with_synth allocs)
            (Texts.join2 " | " "inst=i" title) $
        UiTest.note_spec (inst <> " | <", pitches, [])
    logs0 = snd $ DeriveTest.extract id result
    (events, logs) = DeriveTest.extract_levents id $ Stream.to_list $
        Derive.integrated_events $ head $ Derive.r_integrated result
    (errors, converted) = integrate events
    flatten (note, controls) = note : controls
    extract (Convert.Track title events) =
        (title, map UiTest.extract_event events)

with_synth :: DeriveTest.SimpleAllocations -> DeriveTest.Setup
with_synth allocs =
    DeriveTest.with_synths (DeriveTest.simple_allocs allocs) [Kontakt.synth]

integrate :: [Score.Event] -> ([Convert.Error], Convert.Tracks)
integrate = Convert.integrate (lookup_attrs, Pitch.twelve) tracknums
    where
    lookup_attrs = const mempty
    tracknums = Map.fromList [(UiTest.mk_tid n, n) | n <- [1..10]]

test_integrate :: Test
test_integrate = do
    let f = second (map extract . concatMap flatten) . integrate
        plak = Attrs.attr "plak"
        flatten (note, controls) = note : controls
        extract (Convert.Track title events) =
            (title, map UiTest.extract_event events)
    equal (f [event (0, 1, "4c", [], inst)])
        ([], [(">inst", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
    -- No pitch track, has a control track.
    equal (f [no_pitch (0, 1, [("dyn", [(0, 0), (2, 1)])])])
        ( []
        , [ (">inst", [(0, 1, "")])
          , ("dyn", [(0, 0, "`0x`00"), (2, 0, "`0x`ff")])
          ]
        )
    -- Attributes get mapped back to their call.
    let set = Score.modify_attributes . const
    equal (f [set plak (no_pitch (0, 1, []))])
        ( []
        , [(">inst", [(0, 1, "+plak")])]
        )

    -- Duplicate controls are suppressed.
    equal (f
            [ no_pitch (0, 1, [("dyn", [(0, 1), (1, 1), (2, 0)])])
            , no_pitch (3, 1, [("dyn", [(3, 0)])])
            ])
        ( []
        , [ (">inst", [(0, 1, ""), (3, 1, "")])
          , ("dyn", [(0, 0, "`0x`ff"), (2, 0, "`0x`00")])
          ]
        )
    -- But duplicate pitches are only suppressed when they're on the same note.
    equal (f
            [ pitches (0, 1, [(0, "4c"), (1, "4c"), (2, "4d")])
            , pitches (3, 1, [(3, "4d")])
            ])
        ( []
        , [ (">inst", [(0, 1, ""), (3, 1, "")])
          , ("*", [(0, 0, "4c"), (2, 0, "4d"), (3, 0, "4d")])
          ]
        )
    where
    no_pitch (start, dur, controls) =
        Score.set_pitch mempty $ event (start, dur, "4c", controls, inst)
    pitches (start, dur, pitches) =
        Score.set_pitch (DeriveTest.psignal pitches) $
            event (start, dur, "4c", [], inst)
    event = DeriveTest.mkevent
    inst = "inst"

test_split_overlapping :: Test
test_split_overlapping = do
    let f = map (map extract) . Convert.split_overlapping . map mkevent
        mkevent (s, e) =
            DeriveTest.mkevent (s, e, "4c", [], ScoreT.empty_instrument)
        extract e = (Score.event_start e, Score.event_duration e)
    equal (f [(0, 1), (1, 1)]) [[(0, 1), (1, 1)]]
    equal (f [(0, 1), (0.5, 1), (1, 1)]) [[(0, 1), (1, 1)], [(0.5, 1)]]
    equal (f [(0, 0), (0, 0), (1, 0), (1, 0)])
        [[(0, 0), (1, 0)], [(0, 0), (1, 0)]]
