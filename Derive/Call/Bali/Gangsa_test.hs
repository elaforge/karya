-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Gangsa_test where
import qualified Data.List as List

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.UiTest as UiTest

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Gangsa as Gangsa
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Score as Score

import qualified Instrument.Common as Common
import Global
import Types


test_norot = do
    let run e = derive_extract e title
        title = " | inst-top = (pitch (4f)) | norot-dur=1"
            <> " | norot-arrival=f | initial=t"
        note_inst e = (DeriveTest.e_note e, Score.event_instrument e)
    equal (run note_inst [(2, -2, "norot -- 3a")])
        ([((0, 1, "3a"), pasang), ((1, 1, "3b"), pasang),
            ((2, 1, "3a"), pasang)], [])
    equal (run note_inst [(2, -2, "norot -- 4f")])
        ([((0, 1, "4f"), pasang), ((1, 1, "4e"), pasang),
            ((2, 1, "4f"), pasang)], [])
    equal (run note_inst [(2, -2, "norot _ diamond -- 4c")])
        ([ ((0, 1, "4c"), polos), ((0, 1, "4c"), sangsih)
         , ((1, 1, "4d"), polos), ((1, 1, "3b"), sangsih)
         , ((2, 1, "4c"), polos), ((2, 1, "4c"), sangsih)
         ], [])
    -- Under threshold, split sangsih and polos.
    equal (run note_inst [(2, -2, "kotekan = 2 | norot -- 3a")])
        ([((0, 1, "3a"), polos), ((1, 1, "3b"), sangsih),
            ((2, 1, "3a"), polos)], [])

    -- >norot is a Once pattern.
    equal (run note_inst [(8, -8, "kotekan = 2 | >norot -- 3a")])
        ([ ((5, 1, "3a"), polos), ((5, 1, "3a"), sangsih)
         , ((6, 1, "3a"), polos), ((6, 1, "3a"), sangsih)
         , ((7, 1, "3b"), sangsih)
         , ((8, 1, "3a"), polos)
         ], [])

    equal (run DeriveTest.e_note [(4, -2, "norot -- 3a")])
        ([(2, 1, "3a"), (3, 1, "3b"), (4, 1, "3a")], [])
    equal (run Score.event_flags [(4, -2, "norot -- 3a")])
        ([Gangsa.initial_flag, mempty,
            Flags.infer_duration <> Gangsa.final_flag], [])

    -- Flags aren't messed up from starting at 0.  Also, non-negative duration
    -- is the same as negative.
    equal (run Score.event_flags [(0, 4, "norot -- 3a")])
        ([Gangsa.initial_flag, mempty, mempty, mempty,
            Flags.infer_duration <> Gangsa.final_flag], [])

test_norot_arrival = do
    let run = e_pattern 0 . derive title
        title = " | norot-dur=1 | cancel-pasang 2"
    equal (run [(4, 4, "norot t -- 4c")]) ([(pasang, "-11212121")], [])
    -- The second half of the first call is cancelled out.
    equal (run [(0, 8, "norot f -- 4c"), (8, 4, "norot t -- 4d")])
        ([(pasang, "1212122323232")], [])

    -- Ensure that a 0 dur event at the end of the block still has an arrival.
    -- This actually tests that (ts e) works at the end of the block.
    let run_block notes = e_pattern 0 $
            DeriveTest.derive_blocks [("b1=ruler -- import bali.gangsa",
                UiTest.note_spec (inst_title <> title, notes, []))]
    equal (run_block [(0, 8, "norot f -- 4c"), (8, 0, "norot t -- 4d")])
        ([(pasang, "121212232")], [])

    -- First note is cancelled out.
    equal (run [(0, 2, "norot f -- 4c"), (2, 2, "norot f -- 4d")])
        ([(pasang, "12132")], [])

test_gender_norot = do
    let run = e_pattern 0 . derive ""
    equal (run [(0, 4, "initial=t | gnorot -- 4e")])
        ([(polos, "32123"), (sangsih, "34343")], [])

test_kotekan_flags = do
    let run kotekan = derive_extract extract (ngotek kotekan)
        extract e = (Score.event_start e, Score.event_flags e)
    equal (run True [(0, 4, "k k-121 -- 4c")])
        ([ (0, Gangsa.initial_flag), (1, mempty), (2, mempty)
         , (3, mempty), (3, mempty)
         , (4, Gangsa.final_flag <> Flags.infer_duration)
         ], [])
    equal (run True [(0, 4, "initial=f | final=f | k k-121 -- 4c")])
        ([(1, mempty), (2, mempty), (3, mempty), (3, mempty)], [])

test_kotekan_cancel = do
    -- The final note of the kotekan is cancelled.
    let run = e_pattern 0 . derive (" | cancel-pasang | unison" <> ngotek True)
    -- Base case, with initial and final notes.
    equal (run [(0, 8, "k// -- 4e")])
        ([(polos, "3-3-23-23"), (sangsih, "-2-12-12-")], [])
    -- The unison notes replace the final kotekan note.
    equal (run [(0, 8, "k// -- 4e"), (8, 8, "4f")])
        ([(polos, "3-3-23-24"), (sangsih, "-2-12-124")], [])
    -- Consecutive kotekan calls don't result in doubled notes.
    equal (run [(0, 8, "k k-12-1-21 -- 4c"), (8, 8, "k k-12-1-21 -- 4d")])
        ( [ (polos,     "1-12-1-21-23-2-32")
          , (sangsih,   "-3-23-32-4-34-43-")
          ]
        , []
        )
    -- Sangsih replaces polos.
    equal (run [(0, 8, "k k2-12-12- pat -- 4e"),
            (8, 8, "k k21-21-21 pat -- 4c")])
        ( [ (polos,   "-2-12-12-21-21-21")
          , (sangsih, "3-34-34-3-43-43-4")
          ]
        , []
        )

test_kotekan_infer_duration = do
    let run = derive_polos e_note (" | cancel-pasang | unison" <> ngotek True)
    -- The last note of the kotekan gets an inferred duration to fill the gap.
    equal (run [(0, 4, "k k-121 -- 4c"), (8, 2, "4f")])
        ([(0, 1, "1"), (2, 1, "1"), (3, 1, "2"), (4, 4, "1"), (8, 2, "4")], [])

test_kotekan_irregular = do
    let run kotekan = e_pattern 0 . derive (ngotek kotekan)
    equal (run True [(0, 8, "k_\\ -- 4c")])
        ([(polos, "1-11-1-21"), (sangsih, "4-44-43-4")], [])
    equal (run False [(0, 8, "k_\\ -- 4c")])
        ([(pasang, "1-11-1321")], [])

test_kotekan_regular = do
    let run kotekan = e_pattern 2 . derive (ngotek kotekan)
    -- Start at 2 to avoid accidentally working from 0.
    equal (run True [(2, 8, "k k-12-1-21 -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "-3-23-32-")], [])
    equal (run True [(2, 8, "k k-12-1-21 -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "-3-23-32-")], [])
    equal (run False [(2, 8, "k k-12-1-21 -- 4c")])
        ([(pasang, "131231321")], [])
    equal (run True [(2, 8, "k k-12-1-21 pat -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "434-343-4")], [])
    equal (run False [(2, 8, "k k-12-1-21 pat -- 4c")])
        ([(polos, "131231321"), (sangsih, "434234324")], [])
    equal (run True [(2, 8, "k k-12-1-21 _ d -- 4e")])
        ([(polos, "3-34-3-43"), (sangsih, "323-232-3")], [])
    equal (run False [(2, 8, "k k-12-1-21 _ d -- 4e")])
        ([(pasang, "323423243")], [])
    equal (run True [(2, 8, "k k-12-1-21 pat d -- 4e")])
        ([(polos, "3-34-3-43"), (sangsih, "-2-12-21-")], [])
    equal (run False [(2, 8, "k k-12-1-21 pat d -- 4e")])
        ([(polos, "323423243"), (sangsih, "323123213")], [])

test_kotekan_regular_jalan = do
    -- k// and k\\ work as expected.
    let run kotekan = e_pattern 0
            . derive (" | cancel-pasang" <> ngotek kotekan)
    equal (run True [(0, 8, "k// -- 4e")])
        ([(polos, "3-3-23-23"), (sangsih, "-2-12-12-")], [])
    equal (run True [(0, 8, "k\\\\ -- 4c")])
        ([(polos, "1-1-21-21"), (sangsih, "-2-32-32-")], [])
    equal (run True [(0, 8, "k// -- 4e"), (8, 8, "k\\\\ -- 4c")])
        ([(polos, "3-3-23-23-1-21-21"), (sangsih, "-2-12-12-2-32-32-")], [])

test_kotekan_strange_length = do
    let run start kotekan = e_pattern start . derive (ngotek kotekan)
    equal (run 0 True [(0, 8, "k k-121 -- 4c")])
        ([(polos, "1-121-121"), (sangsih, "-3-2-3-2-")], [])
    strings_like (snd $ run 0 True [(0, 12, "k k-12-21 -- 4c")])
        ["not a multiple of 4"]
    equal (run 0 True [(0, 12, "k '-12-21' -- 4c")])
        ([(polos, "1-12-21-12-21"), (sangsih, "-3-232-3-232-")], [])

test_unison_tuning = do
    -- This is not so much testing the 'unison' call as making sure the
    -- instrument and tuning are properly set.
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup (DeriveTest.with_ui config_inst)
                title
            . UiTest.note_track
        title = "import bali.gangsa | inst = >" <> inst_title
            <> " | scale=wayang | unison"
        extract e = (pretty $ Score.event_instrument e, Score.initial_nn e)
        config_inst = set UiTest.i1 BaliScales.Umbang
            . set UiTest.i2 BaliScales.Isep
        set inst tuning = modify_config inst $
            Common.add_environ EnvKey.tuning tuning
    equal (run [(0, 1, "4i")]) ([(">i1", Just 62.95), (">i2", Just 62.5)], [])

modify_config :: Score.Instrument -> (Common.Config -> Common.Config)
    -> State.State -> State.State
modify_config inst modify = State.allocation inst %= fmap update
    where
    update alloc = alloc
        { StateConfig.alloc_config = modify (StateConfig.alloc_config alloc) }

test_kempyung = do
    let run title = derive_extract extract (title <> " | kempyung")
        extract e = (Score.event_start e, Score.initial_note e)
        notes = [(0, 1, "4c"), (1, 1, "4d")]
    equal (run "" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4g")], [])
    equal (run " | inst-top = (pitch (4f))" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4d")], [])

test_nyogcag = do
    let run = derive_extract extract " | nyog"
        extract e = (Score.event_start e, DeriveTest.e_instrument e)
    let notes = [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
    equal (run notes) ([(0, "i1"), (1, "i2"), (2, "i1")], [])

test_noltol = do
    let run title = e_by_inst extract . derive (" | realize-noltol | " <> title)
        extract e = (Score.event_start e, e_digit_mute e)
    let notes = [(0, 1, "n >i1 -- 4c"), (1, 1, "n >i2 -- 4d"),
            (2, 1, "n >i1 -- 4e")]
    -- 1s of free time between i1
    equal (run "noltol 1.1" notes)
        ([ (polos, [(0, "1"), (2, "3"), (3, "3+")])
         , (sangsih, [(1, "2"), (2, "2+")])
         ], [])
    equal (run "noltol 1" notes)
        ([ (polos, [(0, "1"), (1, "1+"), (2, "3"), (3, "3+")])
         , (sangsih, [(1, "2"), (2, "2+")])
         ], [])

    equal (run "kotekan-dur=2 | noltol 1" [(0, 8, "4c"), (10, 4, "4d")])
        ([(pasang, [(0, "1"), (10, "2")])], [])
    equal (run "kotekan-dur=2 | noltol 1" [(0, 2, "4c"), (4, 4, "4d")])
        ([(pasang, [(0, "1"), (2, "1+"), (4, "2")])], [])

    equal (run "noltol 1 | nyog"
            [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4f")])
        ([ (polos, [(0, "1"), (1, "1+"), (2, "3"), (3, "3+")])
         , (sangsih, [(1, "2"), (2, "2+"), (3, "4"), (4, "4+")])
         ], [])

    -- Output should always be sorted.  TODO I should have quickcheck to
    -- test this in general.
    let run title = DeriveTest.extract Score.event_start
            . derive (" | realize-noltol | " <> title)
    let result = run ("noltol .1" <> ngotek True) [(0, 8, "k k-12-1-21 -- 4c")]
    equal result (first List.sort result)

test_kotekan_cancel_noltol = do
    -- Cancelling works with noltol and kempyung.
    let run postproc = e_by_inst extract . derive (ngotek True <> postproc)
        extract e = (Score.event_start e, e_digit_mute e)

    -- 1+121     => 1+12 +232  >p 1 {final}, >s 2+ {}
    -- -3+2+         3+2+4+3+  >p 2 {initial}
    --     2+232
    --     -4+3+
    let expected =
            ([ (polos,
                [ (0, "1"), (1, "1+")
                , (2, "1"), (3, "2"), (4, "1")
                , (5, "1+"), (6, "2"), (7, "3"), (8, "2"), (9, "2+")
                ])
             , (sangsih,
                [ (1, "3")
                , (2, "3+"), (3, "2"), (4, "2+")
                , (5, "4"), (6, "4+"), (7, "3"), (8, "3+")
                ])
             ], [])
    let notes = [(0, 4, "k k-121 -- 4c"), (4, 4, "k k-121 -- 4d")]
    equal (run " | realize-noltol | noltol | cancel-pasang" notes) expected
    -- 1-121     => 1-121-232
    -- -3-2-        -3-2-4-3
    --     2-232
    --     -4-3-
    equal (run " | realize-noltol | cancel-pasang | noltol" notes) expected

    equal (run " | realize-noltol | noltol | cancel-pasang"
            [(0, 8, "k// -- 4e"), (8, 2, "kempyung | -- 4f")])
        ([ (polos,
            [ (0, "3"), (1, "3+")
            , (2, "3"), (3, "3+"), (4, "2"), (5, "3"), (6, "3+")
            , (7, "2"), (8, "4")
            ])
         , (sangsih,
            [ (1, "2"), (2, "2+"), (3, "1"), (4, "2"), (5, "2+")
            , (6, "1"), (7, "2"), (8, "7")
            ])
         ], [])

ngotek :: Bool -> String
ngotek b = " | kotekan=" <> if b then "2" else "1"

-- * derive

derive_polos :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> ([a], [String])
derive_polos extract title = first (fromMaybe [] . lookup polos)
    . e_by_inst extract . derive title

derive :: String -> [UiTest.EventSpec] -> Derive.Result
derive title notes = derive_tracks $
    UiTest.note_spec (inst_title <> title, notes, [])

derive_extract :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> ([a], [String])
derive_extract extract title = DeriveTest.extract extract . derive title

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = DeriveTest.derive_tracks "import bali.gangsa"

-- * extract

e_by_inst :: (Score.Event -> a) -> Derive.Result
    -> ([(Score.Instrument, [a])], [String])
e_by_inst extract = first Seq.group_fst
    . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))

e_pattern :: RealTime -- ^ expect the first note at this time
    -> Derive.Result
    -> ([(Score.Instrument, String)], [String])
e_pattern start = first (convert_to_pattern pitch_digit start)
    . e_by_inst DeriveTest.e_start_note

convert_to_pattern :: (String -> String) -> RealTime
    -> [(a, [(RealTime, String)])] -> [(a, String)]
convert_to_pattern pitch_digit start inst_notes =
    map (second (as_pattern pitch_digit start end)) inst_notes
    where end = fromMaybe 0 $ Seq.maximum $ map fst $ concatMap snd inst_notes

as_pattern :: (String -> String) -> RealTime -> RealTime
    -> [(RealTime, String)] -> String
as_pattern pitch_digit start end = concat . map format . collect start
    where
    collect at [] = replicate (round (end - at) + 1) []
    -- for each number, collect everything below
    collect at xs = map snd pre : collect (at+1) post
        where (pre, post) = span ((<=at) . fst) xs
    format [] = "-"
    format ns = Seq.join "+" (map pitch_digit ns)

e_note :: Score.Event -> (RealTime, RealTime, String)
e_note e = (Score.event_start e, Score.event_duration e, e_digit e)

e_digit :: Score.Event -> String
e_digit = pitch_digit . DeriveTest.e_pitch

e_digit_mute :: Score.Event -> String
e_digit_mute e = pitch_digit (DeriveTest.e_pitch e)
    <> if Score.has_attribute Attrs.mute e then "+" else ""

pitch_digit :: String -> String
pitch_digit p = case p of
    "4c" -> "1"; "4d" -> "2"; "4e" -> "3"; "4f" -> "4"
    "4g" -> "5"; "4a" -> "6"; "4b" -> "7"
    _ -> "unknown pitch: " <> show p

inst_title :: String
inst_title = "i3 | inst-polos = >i1 | inst-sangsih = >i2"

polos :: Score.Instrument
polos = Score.Instrument "i1"

sangsih :: Score.Instrument
sangsih = Score.Instrument "i2"

pasang :: Score.Instrument
pasang = Score.Instrument "i3"
