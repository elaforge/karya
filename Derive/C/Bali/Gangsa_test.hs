-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Bali.Gangsa_test where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.C.Bali.Gangsa as Gangsa
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import qualified User.Elaforge.Instrument.Kontakt as Kontakt
import qualified User.Elaforge.Instrument.Kontakt.ScGamelan as ScGamelan

import           Global
import           Types
import           Util.Test


-- * norot

test_norot :: Test
test_norot = do
    let run = e_pattern 0 . derive title
        title = " | inst-top = (pitch (4f)) | realize-gangsa 2 | final=t"
    equal (run [(0, 2, "initial=f | norot -- 4c")]) ([(pasang, "-21")], [])
    equal (run [(0, 4, "initial=t | final=t | norot -- 4c")])
        ([(pasang, "12121")], [])
    equal (run [(0, 2, "norot -- 4c")]) ([(pasang, "121")], [])
    equal (run [(0, 4, "norot diamond -- 4d")])
        ([(polos, "23232"), (sangsih, "21212")], [])
    -- Positive defaults to initial=t, negative to initial=f.
    equal (run [(0, 4, "nt -- 4c")]) ([(pasang, "12121")], [])
    equal (run [(4, -4, "nt -- 4c")]) ([(pasang, "-2121")], [])
    -- Under threshold, split sangsih and polos.
    equal (run [(0, 4, "kotekan=2 | norot -- 4c")])
        ([(polos, "1-1-1"), (sangsih, "-2-2-")], [])

    -- Prepare next note.
    equal (run [(0, 4, "i+ | norot -- 4c"), (4, 2, "4e")])
        ([(pasang, "13343")], [])

    -- You can get just a preparation with a short note.
    equal (run [(1, 3, "norot -- 4c"), (4, 2, "4c")]) ([(pasang, "-1121")], [])
    equal (run [(2, 3, "norot -- 4c"), (4, 2, "4c")]) ([(pasang, "--121")], [])
    -- Negative duration implies initial=f so it works too.
    equal (run [(4, -4, "norot -- 4c"), (4, 2, "4c")]) ([(pasang, "-1121")], [])

    equal (run [(0, 4, "i+ | norot -- 4c"), (4, 2, "norot -- 4e")])
        ([(pasang, "1334343")], [])
    -- Unless it doesn't tocuh.
    equal (run [(0, 3, "norot -- 4c"), (4, 2, "4c")]) ([(pasang, "12121")], [])

    -- Goes down because 4f is the top.
    equal (run [(0, 8, "kotekan=2 | norot -- 4c"),
            (8, 4, "kotekan=2 | norot -- 4f")])
        ([(polos, "1-1-144-4-4-4"), (sangsih, "-2-2-443-3-3-")], [])

test_norot_start_prepare :: Test
test_norot_start_prepare = do
    let run kotekan = e_pattern 0
            . derive (" | realize-gangsa 1 " <> ngotek kotekan)
    equal (run False [(0, 4, "nt< -- 4c")]) ([(pasang, "11121")], [])
    equal (run True [(0, 4, "nt< -- 4c")])
        ([(polos, "111-1"), (sangsih, "-112-")], [])
    equal (run True [(0, 8, "nt< -- 4c"), (8, 8, "nt< -- 4e")])
        ([(polos,   "111-133-333-3-3-3"),
          (sangsih, "-112-334-334-4-4-")], [])

test_norot_prepare :: Test
test_norot_prepare = do
    let run = derive " | cancel-pasang 2"
    -- No pitch at 0, but it's not a problem because there's no sustain.
    equal (e_pattern 0 $ run [(4, -4, "norot -- 4c"), (4, 4, "4c")])
        ([(pasang, "-1121")], [])

test_norot_final :: Test
test_norot_final = do
    -- Initial and final get flags.
    let run_flags = DeriveTest.extract Score.event_flags . derive ""
    equal (run_flags [(2, 2, "initial=t | norot -- 3a")])
        ([mempty, mempty, Flags.infer_duration <> Gangsa.final_flag], [])

    -- Infer duration even for simultaneous unison notes.
    let both n = [(polos, n), (sangsih, n)]
    let run title = e_by_inst DeriveTest.e_note
            . derive (" | initial=t" <> title)
    equal (run " | cancel-pasang 2 | unison"
            [(2, 2, "norot -- 4c"), (8, 2, "4d")])
        (both [(2, 1, "4c"), (3, 1, "4d"), (4, 4, "4c"), (8, 2, "4d")], [])
    equal (run (" | cancel-pasang 2 | unison" <> ngotek True)
            [(2, 2, "norot -- 4c"), (8, 2, "4d")])
        ([ (polos, [(2, 1, "4c"), (4, 4, "4c"), (8, 2, "4d")])
         , (sangsih, [(3, 1, "4d"), (8, 2, "4d")])
         ], [])
    -- Also works on pasang.
    equal (run " | unison | cancel-pasang 2"
            [(2, 2, "norot -- 4c"), (8, 2, "4d")])
        (both [(2, 1, "4c"), (3, 1, "4d"), (4, 4, "4c"), (8, 2, "4d")], [])

test_norot_cancel :: Test
test_norot_cancel = do
    let run = e_pattern 0 . derive title
        title = " | initial=t | norot-dur=1 | cancel-pasang 2"
    -- The second half of the first call is cancelled out.
    equal (run [(0, 8, "norot -- 4c"), (8, 4, "norot -- 4d")])
        ([(pasang, "1212122323232")], [])

    -- Canceled by a note at the end of the block.
    let run_block notes = e_pattern 0 $
            DeriveTest.derive_blocks [("b1=ruler -- " <> block_title,
                UiTest.note_spec (title, notes, []))]
    equal (run_block [(0, 8, "norot -- 4c"), (8, 0, "4d")])
        ([(pasang, "121212232")], [])

    -- First note is cancelled out.
    equal (run [(0, 2, "nt- -- 4c"), (2, 2, "nt- -- 4d")])
        ([(pasang, "12132")], [])

test_gender_norot :: Test
test_gender_norot = do
    let run = e_pattern 0 . derive ""
    equal (run [(0, 4, "initial=t | gnorot -- 4e")])
        ([(polos, "32123"), (sangsih, "34343")], [])

test_kotekan_flags :: Test
test_kotekan_flags = do
    let run kotekan = DeriveTest.extract extract . derive (ngotek kotekan)
        extract e = (Score.event_start e, Score.event_flags e)
    equal (run True [(0, 4, "k k-121 -- 4c")])
        ([ (0, mempty), (1, mempty), (2, mempty)
         , (3, mempty), (3, mempty)
         , (4, Gangsa.final_flag <> Flags.infer_duration)
         ], [])
    equal (run True [(0, 4, "initial=f | final=f | k k-121 -- 4c")])
        ([(1, mempty), (2, mempty), (3, mempty), (3, mempty)], [])

test_kotekan_cancel :: Test
test_kotekan_cancel = do
    -- The final note of the kotekan is cancelled.
    let run = e_pattern 0 . derive title
        title = " | cancel-pasang | unison" <> ngotek True
    -- Base case, with initial and final notes.
    equal (run [(0, 8, "k// -- 4e")])
        ([(polos, "3-3-23-23"), (sangsih, "-2-12-12-")], [])
    -- If strong, the unison notes replace the final kotekan note.
    equal (run [(0, 8, "k// -- 4e"), (8, 8, "strong | -- 4f")])
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
    -- Polos replaces both (polos, sangsih) from the unison.
    equal (run [(0, 4, "i- | k k1-21 -- 4c"), (4, 4, "4c")])
        ([(polos, "-1-21"), (sangsih, "--32-")], [])
    -- Same, but ensure it gets the 4 duration.
    let run_notes = e_by_inst DeriveTest.e_note . derive title
    equal (run_notes [(0, 4, "i- | k k1-21 -- 4c"), (4, 4, "4c")])
        ( [ (polos, [(1, 1, "4c"), (3, 1, "4d"), (4, 4, "4c")])
          , (sangsih, [(2, 1, "4e"), (3, 1, "4d")])
          ]
        , []
        )

test_kotekan_infer_duration :: Test
test_kotekan_infer_duration = do
    let run = derive_polos e_note (" | cancel-pasang | unison" <> ngotek True)
    -- The last note of the kotekan gets an inferred duration to fill the gap.
    equal (run [(0, 4, "k k-121 -- 4c"), (8, 2, "4f")])
        ([(0, 1, "1"), (2, 1, "1"), (3, 1, "2"), (4, 4, "1"), (8, 2, "4")], [])

test_kotekan_special :: Test
test_kotekan_special = do
    let run kotekan = e_pattern 0
            . derive (ngotek kotekan <> " | cancel-pasang 2")
    equal (run True [(0, 8, "k_\\ -- 4c")])
        ([(polos, "1-11-1-21"), (sangsih, "4-44-43-4")], [])
    equal (run True [(0, 8, "k-\\ -- 4c")])
        ([(polos, "1211-1-21"), (sangsih, "4-44-43-4")], [])
    equal (run False [(0, 8, "k_\\ telu -- 4c")])
        ([(pasang, "1-11-1321")], [])
    equal (run True [(0, 8, "k_\\ telu -- 4c")])
        ([(polos, "1-11-1-21"), (sangsih, "-3-32-32-")], [])
    equal (run False [(16, -16, "k//\\\\ -- 4e")])
        ([(polos, "--123123213213123"), (sangsih, "--423423243243423")], [])
    equal (run False [(16, -16, "style=telu | k//\\\\ -- 4e")])
        ([(pasang, "--123123213213123")], [])
    equal (run True [(0, 8, "k// -- 4e"), (8, 8, "k\\\\ -- 4c")])
        ([(polos,   "3-3-23-23-1-21-21"),
          (sangsih, "-2-12-12-2-32-32-")], [])
    equal (run True [(8, -8, "k// -- 4e"), (16, -8, "k\\\\ -- 4c")])
        ([(polos,   "--3-23-23-1-21-21"),
          (sangsih, "-2-12-12-2-32-32-")], [])

test_kotekan_regular :: Test
test_kotekan_regular = do
    let run kotekan = e_pattern 2 . derive (ngotek kotekan)
    -- Start at 2 to avoid accidentally working from 0.
    equal (run True [(2, 8, "k k-12_1-21 -- 4c")])
        ([(polos, "1-12-1-21"), (sangsih, "-3-2--32-")], [])
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
    -- The pattern is lined up to the start.
    equal (run False [(2, 12, "k k-12-1-21 -- 4c")])
        ([(pasang, "1312313213123")], [])
    -- inverted
    equal (run True [(2, 8, "k^ k-12-1-21 -- 4e")])
        ([(polos, "3-32-3-23"), (sangsih, "-1-21-12-")], [])
    -- if kotekan ends on 2, then 2 is the destination pitch
    equal (run True [(2, 8, "k k-21-2-12 -- 4e")])
        ([(polos, "3-32-3-23"), (sangsih, "-1-21-12-")], [])

test_kotekan_explicit :: Test
test_kotekan_explicit = do
    let run = e_pattern 2 . derive ""
    equal (run [(2, 4, "ke k-21- k32-0 -- 4c")])
        ([(polos, "--32-"), (sangsih, "-43-1")], [])

test_kotekan_alignment :: Test
test_kotekan_alignment = do
    let run = e_pattern 0 . derive (ngotek False)
    equal (run [(0, 8, "k k-12-1-21 -- 4c")]) ([(pasang, "131231321")], [])
    equal (run [(0, 4, "k k-12-1-21 -- 4c")]) ([(pasang, "13123")], [])
    equal (run [(4, -4, "k k-12-1-21 -- 4c")]) ([(pasang, "-1321")], [])

test_cycles :: Test
test_cycles = do
    equal (Gangsa.cycles (const ['a', 'b', 'c']) [0..4])
        [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'a'), (4, 'b')]
    equal (Gangsa.cycles_end (const ['a', 'b', 'c']) [0..4])
        [(0, 'b'), (1, 'c'), (2, 'a'), (3, 'b'), (4, 'c')]

test_kotekan_regular_negative :: Test
test_kotekan_regular_negative = do
    let run kotekan = e_pattern 8
            . derive (" | cancel-pasang | unison" <> ngotek kotekan)
    -- Default initial=f for negative.
    equal (run True [(16, -8, "k k-12-1-21 --"), (16, 2, "4c")])
        ([(polos, "--12-1-21"), (sangsih, "-3-23-32-")], [])
    equal (run True
            [ (16, -8, "k k-12-1-21 -- 4c")
            , (24, -8, "k k-12-1-21 -- 4d")
            , (24, 1, "4d")
            ])
        ([(polos, "--12-1-21-23-2-32"), (sangsih, "-3-23-32-4-34-43-")], [])

test_kotekan_regular_jalan :: Test
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

test_kotekan_strange_length :: Test
test_kotekan_strange_length = do
    let run start kotekan = e_pattern start . derive (ngotek kotekan)
    equal (run 0 True [(0, 8, "k k-121 -- 4c")])
        ([(polos, "1-121-121"), (sangsih, "-3-2-3-2-")], [])
    strings_like (snd $ run 0 True [(0, 12, "k k-12-21 -- 4c")])
        ["not a multiple of 4"]
    equal (run 0 True [(0, 12, "k '-12-21' -- 4c")])
        ([(polos, "1-12-21-12-21"), (sangsih, "-3-232-3-232-")], [])

test_unison :: Test
test_unison = do
    let run = DeriveTest.extract extract . derive " | unison"
        extract e = (Score.event_instrument e, DeriveTest.e_pitch e)
    equal (run [(0, 1, "4c")]) ([(polos, "4c"), (sangsih, "4c")], [])
    equal (run [(0, 1, "p+ | -- 4c")]) ([(polos, "4c")], [])

test_unison_tuning :: Test
test_unison_tuning = do
    -- This is not so much testing the 'unison' call as making sure the
    -- instrument and tuning are properly set.
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup (DeriveTest.with_ui config_inst)
                title
            . UiTest.note_track
        title = block_title <> " | scale=wayang | unison"
        extract e = (pretty $ Score.event_instrument e, Score.initial_nn e)
        config_inst = set UiTest.i1 BaliScales.Umbang
            . set UiTest.i2 BaliScales.Isep
        set inst tuning = modify_config inst $
            Common.add_cenviron EnvKey.tuning tuning
    equal (run [(0, 1, "4i")]) ([("i1", Just 62.5), ("i2", Just 63)], [])

modify_config :: ScoreT.Instrument -> (Common.Config -> Common.Config)
    -> Ui.State -> Ui.State
modify_config inst modify = Ui.allocation inst %= fmap update
    where
    update alloc = alloc
        { UiConfig.alloc_config = modify (UiConfig.alloc_config alloc) }

test_kempyung :: Test
test_kempyung = do
    let run title = DeriveTest.extract extract . derive (title <> " | kempyung")
        extract e = (Score.event_start e, Score.initial_note e)
        notes = [(0, 1, "4c"), (1, 1, "4d")]
    equal (run "" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4g")], [])
    equal (run " | inst-top = (pitch (4f))" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4d")], [])

test_nyog :: Test
test_nyog = do
    let run title = DeriveTest.extract extract . derive title
        extract e = (Score.event_start e, Score.event_instrument e,
            DeriveTest.e_pitch e)
    equal (run " | nyog" [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")])
        ([(0, polos, "4c"), (1, sangsih, "4d"), (2, polos, "4e")], [])
    equal (run " | nyog" [(0, 1, "s+ -- 4c"), (1, 1, "4d"), (2, 1, "4e")])
        ([(0, sangsih, "4c"), (1, polos, "4d"), (2, sangsih, "4e")], [])

    let runl = DeriveTest.extract extract
            . DeriveTest.derive_tracks_linear block_title
    equal (runl
            ((">i3", [(0, 3, "nyog")]) : UiTest.note_track1 ["4c", "4d", "4e"]))
        ([(0, polos, "4c"), (1, sangsih, "4d"), (2, polos, "4e")], [])

test_nyog_norot :: Test
test_nyog_norot = do
    let run parent notes = DeriveTest.extract extract $
            DeriveTest.derive_tracks_linear (block_title <> ngotek True) $
            (">", parent) : UiTest.note_track notes
        extract e = (Score.event_start e, Score.event_instrument e,
            DeriveTest.e_pitch e)
    equal (run
        [(0, 4, "nyog | ap")]
        [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")])
        ([(0, polos, "4c"), (1, sangsih, "4d"), (2, polos, "4e")], [])
    -- TODO I have to explicitly turn off the final norot note with 'f-', it
    -- seems like that should happen for me.
    equal (run
        [(3, 4, "nyog | ap")]
        [ (0, 3, "f- | nt- -- 4d")
        , (3, 1, "s+ -- 4c"), (4, 1, "4d"), (5, 1, "4e"), (6, 1, "4f")
        ])
        ( [ (0, polos, "4d"), (1, sangsih, "4e"), (2, polos, "4d")
          , (3, sangsih, "4c"), (4, polos, "4d"), (5, sangsih, "4e")
          , (6, polos, "4f")
          ]
        , []
        )

test_nyog_instrument_override :: Test
test_nyog_instrument_override = do
    -- It would be better to not rely on an external instrument, but setting
    -- this up is already really complicated.
    let allocs = ScGamelan.kebyar_allocations "dev"
        synths = [Kontakt.synth]
    let run = extract . DeriveTest.perform_result perform
            . DeriveTest.derive_tracks_setup setup
                ("import bali.gangsa | scale=legong | inst=pemade | nyog")
            . UiTest.note_track
        setup = DeriveTest.with_synths allocs synths
        perform = DeriveTest.perform_synths allocs synths
        extract ((pevents, _), logs) = (map e_event pevents, logs)
        e_event e =
            ( pretty $ Types.patch_name (Types.event_patch e)
            , head $ MSignal.to_pairs $ Types.event_pitch e
            )
    -- The instrument brings tuning along with it, so I match the patch scale
    -- for both polos umbang and sangsih isep.
    equal (run [(0, 1, "4i"), (1, 1, "4i")])
        ([("pemade-p", (0, 60)), ("pemade-s", (1, 60))], [])
    -- If an instrument is given specifically, it overrides.
    equal (run [(0, 1, "s+ -- 4i"), (1, 1, "4i")])
        ([("pemade-s", (0, 60)), ("pemade-p", (1, 60))], [])
    equal (run [(0, 1, "p+ -- 4i"), (1, 1, "4i")])
        ([("pemade-p", (0, 60)), ("pemade-s", (1, 60))], [])

test_noltol :: Test
test_noltol = do
    let run title = e_by_inst extract . derive (" | realize-noltol | " <> title)
        extract e = (Score.event_start e, e_digit_mute e)
    let notes = [(0, 1, "inst=i1 | -- 4c"), (1, 1, "inst=i2 | -- 4d"),
            (2, 1, "inst=i1 | -- 4e")]
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

test_kotekan_cancel_noltol :: Test
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
            [(0, 8, "k// -- 4e"), (8, 2, "kempyung | strong | -- 4f")])
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

ngotek :: Bool -> Text
ngotek b = " | kotekan=" <> if b then "2" else "1"

-- * derive

derive_polos :: (Score.Event -> a) -> Text -> [UiTest.EventSpec]
    -> ([a], [Text])
derive_polos extract title = first (fromMaybe [] . lookup polos)
    . e_by_inst extract . derive title

derive :: Text -> [UiTest.EventSpec] -> Derive.Result
derive title notes = derive_tracks $ UiTest.note_spec (title, notes, [])

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = DeriveTest.derive_tracks block_title

-- * extract

e_by_inst :: (Score.Event -> a) -> Derive.Result
    -> ([(ScoreT.Instrument, [a])], [Text])
e_by_inst extract = first Lists.groupFst
    . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))

e_pattern :: RealTime -- ^ expect the first note at this time
    -> Derive.Result -> ([(ScoreT.Instrument, Text)], [Text])
e_pattern start = first (convert_to_pattern pitch_digit start)
    . e_by_inst DeriveTest.e_start_note

convert_to_pattern :: (Text -> Text) -> RealTime
    -> [(a, [(RealTime, Text)])] -> [(a, Text)]
convert_to_pattern pitch_digit start inst_notes =
    map (second (as_pattern pitch_digit start end)) inst_notes
    where end = fromMaybe 0 $ Lists.maximum $ map fst $ concatMap snd inst_notes

as_pattern :: (Text -> Text) -> RealTime -> RealTime
    -> [(RealTime, Text)] -> Text
as_pattern pitch_digit start end = mconcat . map format . collect start
    where
    collect at [] = replicate (round (end - at) + 1) []
    -- for each number, collect everything below
    collect at xs = map snd pre : collect (at+1) post
        where (pre, post) = span ((<=at) . fst) xs
    format [] = "-"
    format ns = Text.intercalate "+" (map pitch_digit ns)

e_note :: Score.Event -> (RealTime, RealTime, Text)
e_note e = (Score.event_start e, Score.event_duration e, e_digit e)

e_digit :: Score.Event -> Text
e_digit = pitch_digit . DeriveTest.e_pitch

e_digit_mute :: Score.Event -> Text
e_digit_mute e = pitch_digit (DeriveTest.e_pitch e)
    <> if Score.event_duration e == 0 then "+" else ""
    -- Bali instruments should interpret a zero-dur note as a muted stroke.

-- | Count pitches starting from 4c=1.
pitch_digit :: Text -> Text
pitch_digit p = case p of
    "4c" -> "1"; "4d" -> "2"; "4e" -> "3"; "4f" -> "4"
    "4g" -> "5"; "4a" -> "6"; "4b" -> "7"
    _ -> "unknown pitch: " <> showt p

block_title :: Text
block_title =
    "import bali.gangsa | inst = i3 | inst-polos = i1 | inst-sangsih = i2"

polos :: ScoreT.Instrument
polos = "i1"

sangsih :: ScoreT.Instrument
sangsih = "i2"

pasang :: ScoreT.Instrument
pasang = "i3"
